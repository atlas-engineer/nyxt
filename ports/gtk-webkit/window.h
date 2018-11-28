/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#define APPNAME "Next"

#include <webkit2/webkit2.h>

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
	/* {.mod = GDK_MOD2_MASK, .name = "M2"}, */
	/* {.mod = GDK_MOD3_MASK, .name = "M3"}, */
	/* {.mod = GDK_MOD4_MASK, .name = "M4"}, */
	{.mod = GDK_SUPER_MASK, .name = "S"},
	{.mod = GDK_HYPER_MASK, .name = "H"},
	{.mod = GDK_META_MASK, .name = "Meta"},
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
	// We don't kill the buffer since it may be used by other windows.
	g_debug("Delete window %s", window->identifier);

	// Notify the Lisp core.
	{
		GError *error = NULL;
		const char *method_name = "WINDOW-WILL-CLOSE";
		GVariant *window_id = g_variant_new("(s)", window->identifier);
		g_debug("XML-RPC message: %s %s", method_name, g_variant_print(window_id, TRUE));

		SoupMessage *msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
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
			// TODO: g_free(msg) crashes the program, why?
			/* g_free(msg); */
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

// TODO: Not needed?
void window_close_web_view_callback(WebKitWebView *_web_view, Window *window) {
	g_debug("Closing web view");
	window_delete(window);
}

gboolean window_send_event(GtkWidget *_widget, GdkEventKey *event, gpointer data) {
	g_debug("Key pressed:"
		" code %i, symbol %i, name '%s', print '%s'",
		event->hardware_keycode, (gint32)event->keyval,
		gdk_keyval_name(event->keyval), event->string);

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

	Window *window = data;
	GVariant *key_chord = g_variant_new("(isass)",
			event->hardware_keycode,
			keyval_string,
			&builder,
			window->identifier);
	g_debug("XML-RPC message: %s %s", method_name, g_variant_print(key_chord, TRUE));

	SoupMessage *msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
			method_name, key_chord, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return TRUE;
	}

	soup_session_send_message(xmlrpc_env, msg);

	g_debug("Window event XML-RPC response: %s", msg->response_body->data);

	// TODO: Ideally we should receive a boolean.  See on Lisp side.
	GVariant *consumed = soup_xmlrpc_parse_response(msg->response_body->data,
			msg->response_body->length, "i", &error);

	if (error) {
		g_warning("Malformed XML-RPC response: %s", error->message);
		g_error_free(error);
		return TRUE;
	}

	if (!g_variant_get_int32(consumed)) {
		g_debug("Event not consumed, forwarding to GTK");
		return FALSE;
	}

	method_name = "CONSUME-KEY-SEQUENCE";
	GVariant *arg = g_variant_new("(s)",
			window->identifier);
	msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
			method_name, arg, &error);
	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);

	return TRUE;
}

Window *window_init() {
	// TODO: Don't init buffer here?  Re-use last buffer?  Only create if none?
	Buffer *buffer = buffer_init();
	g_debug("Add buffer %p with view %p to window", buffer, buffer->web_view);
	// Make sure that when the browser area becomes visible, it will get
	// mouse and keyboard events
	gtk_widget_grab_focus(GTK_WIDGET(buffer->web_view));

	Minibuffer *minibuffer = minibuffer_init();
	// TODO: What default shall we use for the minibuffer height?
	gtk_widget_set_size_request(GTK_WIDGET(minibuffer->web_view), -1, 32);

	GtkWidget *mainbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(mainbox), GTK_WIDGET(buffer->web_view), TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(mainbox), GTK_WIDGET(minibuffer->web_view), FALSE, FALSE, 0);

	Window *window = calloc(1, sizeof (Window));
	// Create an 800x600 window that will contain the browser instance
	window->base = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window->base), 800, 600);
	// TODO: Make title customizable from Lisp.
	gtk_window_set_title(GTK_WINDOW(window->base), APPNAME);
	// TODO: Deprecated?
	/* gtk_window_set_wmclass(GTK_WINDOW(main_window), APPNAME, APPNAME); */

	// Put the browser area into the main window
	gtk_container_add(GTK_CONTAINER(window->base), mainbox);

	// Set up callbacks so that if either the main window or the browser
	// instance is closed, it is handled properly.
	g_signal_connect(window->base, "destroy", G_CALLBACK(window_destroy_callback), window);
	g_signal_connect(buffer->web_view, "close", G_CALLBACK(window_close_web_view_callback), window);

	// TODO: Connect to mainbox or window?
	// TODO: send event on press and/or release?
	g_signal_connect(minibuffer->web_view, "key-press-event", G_CALLBACK(window_send_event), window);
	g_signal_connect(mainbox, "key-press-event", G_CALLBACK(window_send_event), window);
	/* g_signal_connect(mainbox, "key-release-event", G_CALLBACK(window_send_event), NULL); */

	// Make sure the main window and all its contents are visible
	gtk_widget_show_all(window->base);

	window->buffer = buffer;
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

	window->buffer = buffer;
	g_debug("New active buffer %p with view %p", buffer, buffer->web_view);
	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));
	g_debug("Remove buffer view %p from window", box_children->data);
	gtk_container_remove(GTK_CONTAINER(mainbox), GTK_WIDGET(box_children->data));

	gtk_box_pack_start(GTK_BOX(mainbox), GTK_WIDGET(buffer->web_view), TRUE, TRUE, 0);

	gtk_widget_grab_focus(GTK_WIDGET(buffer->web_view));
	g_signal_connect(buffer->web_view, "key-press-event", G_CALLBACK(window_send_event), window);

	// We don't show all widgets, otherwise it would re-show the minibuffer if it
	// was hidden.
	gtk_widget_show(GTK_WIDGET(buffer->web_view));
}

gint64 window_set_minibuffer_height(Window *window, gint64 height) {
	g_debug("Resize window %p minibuffer", window);
	if (height == 0) {
		gtk_widget_hide(GTK_WIDGET(window->minibuffer->web_view));
		return 0;
	}

	gtk_widget_set_size_request(GTK_WIDGET(window->minibuffer->web_view), -1, height);
	gtk_widget_show(GTK_WIDGET(window->minibuffer->web_view));
	window->minibuffer_height = height;

	gint natural_height;
	gtk_widget_get_preferred_height(GTK_WIDGET(window->minibuffer->web_view), NULL, &natural_height);
	return natural_height;
}
