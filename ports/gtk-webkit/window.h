/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#define APPNAME "Next"

#include <webkit2/webkit2.h>
#include <gdk/gdkkeysyms-compat.h>

#include "buffer.h"
#include "minibuffer.h"
#include "server-state.h"
#include "client.h"

typedef struct {
	int mod;
	char *name;
} Modifier;

// See the documentation of "enum GdkModifierType".
static Modifier modifier_names[] = {
	{.mod = GDK_SHIFT_MASK, .name = "s"},
	{.mod = GDK_LOCK_MASK, .name = "Lock"},
	{.mod = GDK_CONTROL_MASK, .name = "C"},
	{.mod = GDK_MOD1_MASK, .name = "M"}, // Usually "Alt".
	// M is sometimes also seen as M2-M4, which can confuse the Lisp side.
	// I'm not sure that we need M2-M4 at all.
	/* {.mod = GDK_MOD2_MASK, .name = "M2"}, */
	/* {.mod = GDK_MOD3_MASK, .name = "M3"}, */
	/* {.mod = GDK_MOD4_MASK, .name = "M4"}, */
	{.mod = GDK_SUPER_MASK, .name = "S"},
	{.mod = GDK_HYPER_MASK, .name = "H"},
	{.mod = GDK_META_MASK, .name = "Meta"},
};

static guint key_blacklist[] = {
	GDK_ISO_Level2_Latch,
	GDK_ISO_Level3_Shift,
	GDK_ISO_Level3_Latch,
	GDK_ISO_Level3_Lock,
	GDK_ISO_Level5_Shift,
	GDK_ISO_Level5_Latch,
	GDK_ISO_Level5_Lock,
	GDK_ISO_Group_Shift,
	GDK_ISO_Group_Latch,
	GDK_ISO_Group_Lock,
	GDK_ISO_Next_Group,
	GDK_ISO_Next_Group_Lock,
	GDK_ISO_Prev_Group,
	GDK_ISO_Prev_Group_Lock,
	GDK_ISO_First_Group,
	GDK_ISO_First_Group_Lock,
	GDK_ISO_Last_Group,
	GDK_ISO_Last_Group_Lock,
};

typedef struct {
	GtkWidget *base;
	Buffer *buffer;
	char *identifier;
	Minibuffer *minibuffer;
	int minibuffer_height;
} Window;

void window_destroy_callback(GtkWidget *_widget, Window *window) {
	g_debug("Signal callback to destroy window %s", window->identifier);
	akd_remove_object_for_key(state.windows, window->identifier);
}

void window_delete(Window *window) {
	// TODO: Why do we need to remove the buffer from the window to prevent a web
	// view corruption?
	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));
	g_debug("Remove buffer view %p from window", box_children->data);
	gtk_container_remove(GTK_CONTAINER(mainbox), GTK_WIDGET(box_children->data));

	{
		// Notify the Lisp core.
		GError *error = NULL;
		const char *method_name = "WINDOW-WILL-CLOSE";
		GVariant *window_id = g_variant_new("(s)", window->identifier);
		g_message("XML-RPC message: %s %s", method_name, g_variant_print(window_id, TRUE));

		SoupMessage *msg = soup_xmlrpc_message_new(state.core_socket,
				method_name, window_id, &error);

		if (error) {
			g_warning("Malformed XML-RPC message: %s", error->message);
			g_error_free(error);
			// If the XML-RPC request fails, we should still close the window on the GTK
			// side.
		} else {
			// Send synchronously so that if this is the last window, we don't quit
			// GTK before actually sending the message.
			soup_session_send_message(xmlrpc_env, msg);
			// 'msg' is freed automatically.
		}
	}

	if (window->base != NULL && !gtk_widget_in_destruction(window->base)) {
		// If window was destroyed externally, then this is already done.
		g_debug("Destroy window widget %s", window->identifier);
		gtk_widget_destroy(window->base);
	}

	minibuffer_delete(window->minibuffer);

	g_free(window->identifier);
	g_free(window);

	if (g_hash_table_size(state.windows->dict) >= 1) {
		return;
	}

	// No more windows, let's quit.
	// TODO: This is dirty, since it could interupt the request response of
	// server_window_delete.  We probably need add a "quit" request to the API.
	g_debug("No more windows, quitting");
	gtk_main_quit();
}

gboolean window_consume_event(SoupSession *session, SoupMessage *msg, gpointer window_data) {
	GError *error = NULL;
	g_debug("Window event XML-RPC response: %s", msg->response_body->data);

	// TODO: Ideally we should receive a boolean.  See on Lisp side.
	GVariant *consumed = soup_xmlrpc_parse_response(msg->response_body->data,
			msg->response_body->length, "i", &error);

	if (error) {
		g_warning("%s: '%s'", error->message,
			strndup(msg->response_body->data, msg->response_body->length));
		g_error_free(error);
		return TRUE;
	}

	if (!g_variant_get_int32(consumed)) {
		g_debug("Event not consumed, forwarding to GTK");
		return FALSE;
	}

	Window *window = window_data;
	const char *method_name = "CONSUME-KEY-SEQUENCE";
	GVariant *id = g_variant_new("(s)",
			window->identifier);
	g_message("XML-RPC message: %s, window id %s", method_name, window->identifier);
	msg = soup_xmlrpc_message_new(state.core_socket,
			method_name, id, &error);
	// TODO: There is a possible race condition here: if two keys are pressed very
	// fast before the first CONSUME-KEY-SEQUENCE is received by the Lisp core,
	// then when the first CONSUME-KEY-SEQUENCE is received, *key-chord-stack*
	// will contain the two keys.  The second CONSUME-KEY-SEQUENCE will have an
	// empty *key-chord-stack*.  In practice, those key presses would have to be
	// programmatically generated at the system level, so it's mostly a non-issue.
	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
}

gboolean window_send_event(GtkWidget *_widget, GdkEventKey *event, gpointer window_data) {
	g_debug("Key pressed:"
		" code %i, symbol %i, name '%s', print '%s, is_modifier %i",
		event->hardware_keycode, (gint32)event->keyval,
		gdk_keyval_name(event->keyval), event->string, event->is_modifier);

	// Don't pass modifiers alone, otherwise the core could see them as self-inserting
	// character, which would print "Control_L" and the like in the minibuffer.
	if (event->is_modifier) {
		// Forward to GTK.
		return FALSE;
	}

	// Don't pass GDK_ISO_Level3_Shift and the like, those (non-modifier) keys
	// alter the keymap upstream, we only want the result.
	for (int i = 0; i < (sizeof key_blacklist)/(sizeof key_blacklist[0]); i++) {
		if (event->keyval == key_blacklist[i]) {
			// Forward to GTK.
			return FALSE;
		}
	}

	// Translate ISO_Left_Tab to shift-TAB.
	if (event->keyval == GDK_ISO_Left_Tab) {
		event->keyval = GDK_Tab;
		event->state |= GDK_SHIFT_MASK;
	}

	GError *error = NULL;
	const char *method_name = "PUSH-KEY-EVENT";

	// event->string is deprecated but it's very much what we want.
	// For characters like Escape, this value is '\u001b', which is understood by
	// s-xml-rpc as 0x1b, so we are fine.
	gchar *keyval_string = event->string;
	if (event->state & GDK_CONTROL_MASK &&
		(((event->keyval >= 'A') && (event->keyval <= 'Z')) ||
		((event->keyval >= 'a') && (event->keyval <= 'z')))) {
		// The control modifier turns the A-Za-z event->string into ASCII control
		// characters.
		keyval_string = gdk_keyval_name(event->keyval);
	} else if (keyval_string[0] == '\0') {
		// Some keys like F1 don't have a printed representation, so we send the
		// associated GDK symbol then.  On the Lisp side, it can be associated with
		// the proper string via set-gtk-conversion-table.
		keyval_string = gdk_keyval_name(event->keyval);
	}

	GVariantBuilder builder;
	g_variant_builder_init(&builder, G_VARIANT_TYPE("as"));
	for (int i = 0; i < (sizeof modifier_names)/(sizeof modifier_names[0]); i++) {
		if (event->state & modifier_names[i].mod) {
			g_variant_builder_add(&builder, "s", modifier_names[i].name);
		}
	}

	Window *window = window_data;
	GVariant *key_chord = g_variant_new("(isass)",
			event->hardware_keycode,
			keyval_string,
			&builder,
			window->identifier);
	g_message("XML-RPC message: %s %s = %s", method_name, "(keycode, keyval, modifiers, window id)", g_variant_print(key_chord, TRUE));

	SoupMessage *msg = soup_xmlrpc_message_new(state.core_socket,
			method_name, key_chord, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return TRUE;
	}

	soup_session_queue_message(xmlrpc_env, msg, (SoupSessionCallback)window_consume_event, window);
	return TRUE;
}

Window *window_init() {
	Minibuffer *minibuffer = minibuffer_init();
	// TODO: Initial minibuffer size must be set here or else it will stick to 0.
	// This seems to be related to the resizing issue below.
	gtk_widget_set_size_request(GTK_WIDGET(minibuffer->web_view), -1, 200);

	GtkWidget *mainbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_end(GTK_BOX(mainbox), GTK_WIDGET(minibuffer->web_view), FALSE, FALSE, 0);

	Window *window = calloc(1, sizeof (Window));
	// Create an 800x600 window that will contain the browser instance
	window->base = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window->base), 800, 600);

	// Deprecated, but we want it to be "Next", not "Next-gtk-webkit".
	gtk_window_set_wmclass(GTK_WINDOW(window->base), g_string_ascii_down(g_string_new(APPNAME))->str, APPNAME);

	// Put the browser area into the main window
	gtk_container_add(GTK_CONTAINER(window->base), mainbox);

	// Set up callbacks so that if either the main window or the browser
	// instance is closed, it is handled properly.
	g_signal_connect(window->base, "destroy", G_CALLBACK(window_destroy_callback), window);

	// We only send key-press events, since we don't need such fine-grained tuning
	// from the Lisp side.
	g_signal_connect(window->base, "key-press-event", G_CALLBACK(window_send_event), window);

	// Make sure the main window and all its contents are visible
	gtk_widget_show_all(window->base);

	window->minibuffer = minibuffer;
	return window;
}

void window_set_active_buffer(Window *window, Buffer *buffer) {
	if (window == NULL) {
		g_warning("Non-existent window");
		return;
	}
	if (buffer == NULL) {
		g_warning("Non-existent buffer");
		return;
	}

	// When window is first set up, there is only a minibuffer.
	char *previous_buffer_id = NULL;
	if (window->buffer != NULL) {
		previous_buffer_id = window->buffer->identifier;
	}
	g_message("Window %s switches from buffer %s to %s",
		window->identifier, previous_buffer_id, buffer->identifier);

	window->buffer = buffer;

	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));
	if (g_list_length(box_children) > 1) {
		g_debug("Remove buffer view %p from window", box_children->data);
		gtk_container_remove(GTK_CONTAINER(mainbox), GTK_WIDGET(box_children->data));
	}

	gtk_box_pack_start(GTK_BOX(mainbox), GTK_WIDGET(buffer->web_view), TRUE, TRUE, 0);

	gtk_widget_grab_focus(GTK_WIDGET(buffer->web_view));

	// We don't show all widgets, otherwise it would re-show the minibuffer if it
	// was hidden.
	gtk_widget_show(GTK_WIDGET(buffer->web_view));
}

gint64 window_set_minibuffer_height(Window *window, gint64 height) {
	g_message("Window %s resizes its minibuffer to %li", window->identifier, height);
	if (height == 0) {
		gtk_widget_hide(GTK_WIDGET(window->minibuffer->web_view));
		return 0;
	}

	// TODO: Changing the size request of an existing object does not seem to work here.
	gtk_widget_set_size_request(GTK_WIDGET(window->minibuffer->web_view), -1, 200);
	gtk_widget_set_size_request(GTK_WIDGET(window->minibuffer->web_view), -1, height);
	gtk_widget_show(GTK_WIDGET(window->minibuffer->web_view));
	window->minibuffer_height = height;

	gint minimum_height;
	gint natural_height;
	gtk_widget_get_preferred_height(GTK_WIDGET(window->minibuffer->web_view), &minimum_height, &natural_height);
	g_debug("minimum height %li, natural_height %li", minimum_height, natural_height);
	return natural_height;
}

void window_set_title(Window *window, const char *title) {
	gtk_window_set_title(GTK_WINDOW(window->base), title);
}
