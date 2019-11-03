/*
Copyright © 2018-2019 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.

Web views can resize their parent widget, and since we have multiple web views
in a window, we need to make sure they have separate parent widgets or else the
resize hints would conflict.

The widget hierarchy:

- GtkWindow
  - GtkBox
    - GtkBox
      - Web view (buffer)
    - GtkBox
      - Web view (minibuffer)
*/
#pragma once

#define APPNAME "Next"

#include <webkit2/webkit2.h>
#include <gdk/gdkkeysyms-compat.h>

#include "buffer.h"
#include "minibuffer.h"
#include "server-state.h"

typedef struct {
	char *old;
	char *new;
} KeyTranslation;

static KeyTranslation key_translations[] = {
	{.old = "BackSpace", .new = "BACKSPACE"},
	{.old = " ", .new = "SPACE"},
	{.old = "space", .new = "SPACE"},
	{.old = "Delete", .new = "DELETE"},
	{.old = "minus", .new = "HYPHEN"},
	{.old = "Escape", .new = "ESCAPE"},
	{.old = "Return", .new = "RETURN"},
	{.old = "Tab", .new = "TAB"},
};

guint window_string_to_modifier(gchar *s) {
	for (int i = 0; i < (sizeof modifier_names)/(sizeof modifier_names[0]); i++) {
		if (g_strcmp0(modifier_names[i].name, s) == 0) {
			return modifier_names[i].mod;
		}
	}
	return 0;
}

typedef struct {
	GtkWidget *base;
	GtkEntry *key_string_buffer;
	Buffer *buffer;
	char *identifier;
	Minibuffer *minibuffer;
	int minibuffer_height;
	gboolean notify_synchronously;
} Window;

typedef struct {
	GdkEvent event; // Must be a copy, not a pointer since the event can be freed.
	Window *window;
} WindowEvent;

void window_destroy_callback(GtkWidget *_widget, Window *window) {
	g_debug("Signal callback to destroy window %s", window->identifier);
	g_hash_table_remove(state.windows, window->identifier);
}

void window_delete(Window *window) {
	// TODO: Why do we need to remove the buffer from the window to prevent a web
	// view corruption?
	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));
	GtkWidget *topbox = GTK_WIDGET(box_children->data);
	GList *topbox_children = gtk_container_get_children(GTK_CONTAINER(topbox));
	g_debug("Remove buffer view %p from window", topbox_children->data);
	gtk_container_remove(GTK_CONTAINER(topbox), GTK_WIDGET(topbox_children->data));

	{
		// Notify the Lisp core.
		GError *error = NULL;
		const char *method_name = "window_will_close";
		GVariant *window_id = g_variant_new("(s)", window->identifier);
		g_message("RPC message: %s %s", method_name, g_variant_print(window_id, TRUE));

		if (window->notify_synchronously) {
			// Send synchronously so that if this is the last window, we don't quit
			// GTK before actually sending the message.
			g_dbus_connection_call_sync(state.connection,
				CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
				method_name,
				window_id,
				NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL, &error);
		} else {
			// If Lisp core instructed to close the window, then it is still
			// synchronously waiting on the RPC return at this point, so we can't send
			// synchronously lest we enter a dead lock.  But that's OK, because the
			// Lisp core should never delete the last window.
			g_dbus_connection_call(state.connection,
				CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
				method_name,
				window_id,
				NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL, NULL);
		}

		if (error != NULL) {
			g_warning("Error in RPC call: %s", error->message);
			g_error_free(error);
			// If the RPC request fails, we should still close the window on the GTK
			// side.
		}
	}

	minibuffer_delete(window->minibuffer);

	g_free(window->identifier);
	g_free(window);

	if (g_hash_table_size(state.windows) >= 1) {
		return;
	}

	// No more windows, let's quit.
	// TODO: This is dirty, since it could interupt the request response of
	// server_window_delete.  We probably need add a "quit" request to the API.
	g_debug("No more windows, quitting");
	gtk_main_quit();
}

void window_generate_input_event(WindowEvent *window_event) {
	// We need to generate key press events programmatically from the call back
	// when the key press was not consumed.  The original event might have been
	// freed, which is why we need to copy it's content into the callback
	// payload (window_data).
	g_debug("Event not consumed, forwarding to GTK");

	GdkEvent *event = gdk_event_new(window_event->event.type);

	// REVIEW: Set window to base or buffer->web_view ?
	event->any.window =
		g_object_ref(gtk_widget_get_window(GTK_WIDGET(window_event->window->base)));

	// The "send_event" field is used to mark the event as an "unconsumed"
	// keypress.  The distinction allows us to avoid looping indefinitely.
	event->any.send_event = TRUE;

	switch (window_event->event.type) {
	case GDK_KEY_PRESS:
	case GDK_KEY_RELEASE: {
		gchar *type = "press";
		if (window_event->event.type == GDK_KEY_RELEASE) {
			type = "release";
		}
		g_debug("Generating key %s", type);
		// REVIEW: Seems that there is no event time at this point,
		// gtk_get_current_event_time() and GDK_CURRENT_TIME return 0.
		event->key.time = GDK_CURRENT_TIME;
		GdkEventKey *e = (GdkEventKey *)&window_event->event;
		event->key.state = e->state;
		event->key.keyval = e->keyval;
		if (e->string != NULL) {
			event->key.string = e->string;
			event->key.length = strlen(event->key.string);
		}
		event->key.hardware_keycode = e->hardware_keycode;
		event->key.group = e->group;
		event->key.is_modifier = e->is_modifier;
		break;
	}
	case GDK_BUTTON_PRESS:
	case GDK_BUTTON_RELEASE: {
		event->button.window =
			g_object_ref(gtk_widget_get_window(GTK_WIDGET(window_event->window->buffer->web_view)));
		gchar *type = "press";
		if (window_event->event.type == GDK_BUTTON_RELEASE) {
			type = "release";
		}
		g_debug("Generating button %s", type);
		event->button.time = GDK_CURRENT_TIME;
		GdkEventButton *e = (GdkEventButton *)&window_event->event;
		event->button.button = e->button;
		event->button.state = e->state;
		event->button.x = e->x;
		event->button.y = e->y;
		break;
	}
	case GDK_SCROLL: {
		event->button.window =
			g_object_ref(gtk_widget_get_window(GTK_WIDGET(window_event->window->buffer->web_view)));
		g_debug("Generating scroll press");
		event->button.time = GDK_CURRENT_TIME;
		GdkEventScroll *e = (GdkEventScroll *)&window_event->event;
		event->scroll.direction = e->direction;
		event->scroll.state = e->state;
		event->scroll.delta_x = e->delta_x;
		event->scroll.delta_y = e->delta_y;
		event->scroll.x = e->x;
		event->scroll.y = e->y;
		break;
	}
	}

	GdkDevice *device = NULL;
	GdkDisplay *display = gdk_display_get_default();
	GdkSeat *seat = gdk_display_get_default_seat(display);
	device = gdk_seat_get_keyboard(seat);
	gdk_event_set_device(event, device);

	gtk_main_do_event(event);
	gdk_event_free(event);
	g_free(window_event);
}

gboolean window_send_event(gpointer window_data,
	gchar *event_string, guint modifiers,
	guint16 hardware_keycode, guint keyval,
	gdouble x, gdouble y,
	gboolean released) {
	if (window_data == NULL) {
		g_error("window_send_event called with null window");
		return FALSE;
	}

	GVariantBuilder builder;
	g_variant_builder_init(&builder, G_VARIANT_TYPE("as"));
	for (int i = 0; i < (sizeof modifier_names)/(sizeof modifier_names[0]); i++) {
		if (modifiers & modifier_names[i].mod) {
			g_variant_builder_add(&builder, "s", modifier_names[i].name);
		}
	}
	if (released) {
		g_variant_builder_add(&builder, "s", "R");
	}

	const char *method_name = "push_input_event";
	Window *window = window_data;
	GVariant *key_chord = g_variant_new("(isasddis)",
			hardware_keycode,
			event_string,
			&builder,
			x, y,
			keyval,
			window->identifier);
	g_message("RPC message: %s %s = %s",
		method_name,
		"(keycode, keystring, modifiers, x, y, low level data, window id)",
		g_variant_print(key_chord, TRUE));

	// Leave input event generation to the Lisp.
	g_dbus_connection_call(state.connection,
		CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
		method_name,
		key_chord,
		NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL, NULL);
	return TRUE;
}

#define INPUT_IS_NOT_POINTER -1
#define INPUT_IS_NOT_PRINTABLE -1
#define INPUT_IS_PRINTABLE -2

gboolean window_key_event(GtkWidget *_widget, GdkEventKey *event, gpointer window_data) {
	{
		gchar *type = "pressed";
		if (event->type == GDK_KEY_RELEASE) {
			type = "released";
		}
		g_debug("Key %s:"
			" code %i, symbol %i, name '%s', print '%s', is_modifier %i, explicitly %i, time %u",
			type,
			event->hardware_keycode, (gint32)event->keyval,
			gdk_keyval_name(event->keyval), event->string, event->is_modifier,
			event->send_event, event->time);
	}

	if (event->send_event) {
		// This event was generated from a non-generated keypress unconsumed by the
		// Lisp core.  This must be the first test.
		g_debug("Forward unconsumed event to GTK");
		return FALSE;
	}

	Window *window = window_data;

	// Generate the result of the current keypress into the dummy
	// key_string_buffer (a GtkEntry that's never shown on screen) so that we can
	// collect the printed representation of composed keypress, such as dead keys.
	// This must come right after the unconsumed event test.
	gtk_entry_im_context_filter_keypress(window->key_string_buffer, event);

	// Translate ISO_Left_Tab to shift-TAB.
	if (event->keyval == GDK_ISO_Left_Tab) {
		event->keyval = GDK_Tab;
		event->state |= GDK_SHIFT_MASK;
	}

	gchar *keyval_string = gdk_keyval_name(event->keyval);
	gdouble type = INPUT_IS_NOT_PRINTABLE;

	if (gtk_entry_get_text_length(window->key_string_buffer) >= 1) {
		type = INPUT_IS_PRINTABLE;
		keyval_string = strdup(gtk_entry_get_text(window->key_string_buffer));
		gtk_entry_set_text(window->key_string_buffer, "");
	}

	// If at this point the keyval_string is not standard, we use the key
	// translation to set it to a standard name.
	for (int i = 0; i < (sizeof key_translations)/(sizeof key_translations[0]); i++) {
		if (g_strcmp0(keyval_string, key_translations[i].old) == 0) {
			keyval_string = key_translations[i].new;
			break;
		}
	}

	// C-[ and C-] are turned to \u001b and \u001d (escape) respectively.  Fix this.
	if (g_strcmp0(gdk_keyval_name(event->keyval), "bracketleft") == 0) {
		keyval_string = "[";
	} else if (g_strcmp0(gdk_keyval_name(event->keyval), "bracketright") == 0) {
		keyval_string = "]";
	}

	return window_send_event(window_data,
		       keyval_string, event->state,
		       event->hardware_keycode, event->keyval,
		       INPUT_IS_NOT_POINTER, type, // 'type' is used to pass extra information such as "is printable?".
		       event->type == GDK_KEY_RELEASE);
}

gboolean window_button_event(GtkWidget *_widget, GdkEventButton *event, gpointer buffer_data) {
	{
		gchar *type = "pressed";
		if (event->type == GDK_BUTTON_RELEASE) {
			type = "released";
		}
		g_debug("Button %s:"
			" type %i, button %u, root coord (%g, %g), rel coord (%g, %g), modifiers %i, explicitly %i, time %u",
			type,
			event->type,
			event->button,
			event->x_root, event->y_root,
			event->x, event->y,
			event->state,
			event->send_event, event->time);
	}
	if (event->send_event) {
		// This event was generated from a non-generated keypress unconsumed by the
		// Lisp core.  This must be the first test.
		g_debug("Forward unconsumed event to GTK");
		return FALSE;
	}

	Window *window = NULL;
	GHashTableIter iter;
	gpointer key, value;
	g_hash_table_iter_init(&iter, state.windows);
	while (g_hash_table_iter_next(&iter, &key, &value)) {
		Window *w = (Window *)value;
		if (w->buffer == buffer_data || w->minibuffer == buffer_data) {
			window = w;
			break;
		}
	}

	gchar *event_string = g_strdup_printf("button%d", event->button);
	return window_send_event(window,
		       event_string, event->state,
	               // We pass no hardware keycode for now (0), maybe some day.
		       0, event->button,
		       event->x, event->y,
		       event->type == GDK_BUTTON_RELEASE);
}

gboolean window_scroll_event(GtkWidget *_widget, GdkEventScroll *event, gpointer buffer_data) {
	g_debug("Scroll event:"
		" type %i, direction %i, root coord (%g, %g), rel coord (%g, %g), delta (%g, %g), modifiers %i, explicitly %i, time %u",
		event->type,
		event->direction,
		event->x_root, event->y_root,
		event->x, event->y,
		event->delta_x, event->delta_y,
		event->state, event->send_event, event->time);

	if (event->send_event) {
		// This event was generated from a non-generated keypress unconsumed by the
		// Lisp core.  This must be the first test.
		g_debug("Forward unconsumed event to GTK");
		return FALSE;
	}

	Window *window = NULL;
	GHashTableIter iter;
	gpointer key, value;
	g_hash_table_iter_init(&iter, state.windows);
	while (g_hash_table_iter_next(&iter, &key, &value)) {
		Window *w = (Window *)value;
		if (w->buffer == buffer_data || w->minibuffer == buffer_data) {
			window = w;
			break;
		}
	}

	guint button = 0;
	switch (event->direction) {
	case GDK_SCROLL_UP:
		button = 4;
		break;
	case GDK_SCROLL_DOWN:
		button = 5;
		break;
	case GDK_SCROLL_LEFT:
		button = 6;
		break;
	case GDK_SCROLL_RIGHT:
		button = 7;
		break;
	case GDK_SCROLL_SMOOTH: {
		if (event->delta_y <= 0) {
			button = 4;
		} else if (event->delta_y > 0) {
			button = 5;
		} else if (event->delta_x <= 0) {
			button = 6;
		} else if (event->delta_x > 0) {
			button = 7;
		}
		break;
	}
	}

	gchar *event_string = g_strdup_printf("button%d", button);
	return window_send_event(window,
		       event_string, event->state,
	               // We pass the direction as low-level data so that we can
	               // reconstruct the event when forwarded back to the platform
	               // port.
		       event->direction, button,
		       event->x, event->y,
		       false);
}

Window *window_init() {
	Minibuffer *minibuffer = minibuffer_init();

	GtkWidget *mainbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	GtkWidget *bottombox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_end(GTK_BOX(bottombox), GTK_WIDGET(minibuffer->web_view), TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(mainbox), GTK_WIDGET(bottombox), FALSE, FALSE, 0);

	Window *window = calloc(1, sizeof (Window));
	if (window == NULL) {
		g_error("Failed to allocate window");
		exit(1);
	}

	window->notify_synchronously = true;

	// Create an 800x600 window that will contain the browser instance
	window->base = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window->base), 800, 600);

	// Key string buffer is used to store the key event characters.
	window->key_string_buffer = GTK_ENTRY(gtk_entry_new());

	// Deprecated, but we want it to be "Next", not "Next-gtk-webkit".
	gtk_window_set_wmclass(GTK_WINDOW(window->base), g_string_ascii_down(g_string_new(APPNAME))->str, APPNAME);

	// Put the browser area into the main window
	gtk_container_add(GTK_CONTAINER(window->base), mainbox);

	// Set up callbacks so that if either the main window or the browser
	// instance is closed, it is handled properly.
	g_signal_connect(window->base, "destroy", G_CALLBACK(window_destroy_callback), window);

	// Only key events can be captured here, button events are captured by the web view in the buffer.
	g_signal_connect(window->base, "key-press-event", G_CALLBACK(window_key_event), window);
	g_signal_connect(window->base, "key-release-event", G_CALLBACK(window_key_event), window);

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

	{
		// When window is first set up, there is only a minibuffer.
		char *previous_buffer_id = NULL;
		if (window->buffer != NULL) {
			previous_buffer_id = window->buffer->identifier;
		}
		g_message("Window %s switches from buffer %s to %s with URI %s",
			window->identifier, previous_buffer_id, buffer->identifier,
			webkit_web_view_get_uri(buffer->web_view));
	}

	window->buffer = buffer;

	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));

	if (g_list_length(box_children) > 1) {
		GtkWidget *topbox = GTK_WIDGET(box_children->data);
		g_object_ref(topbox); // TODO: Do we need to keep a reference here?
		g_debug("Remove buffer view %p from window", topbox);
		GList *topbox_children = gtk_container_get_children(GTK_CONTAINER(topbox));
		if (topbox_children) {
			WebKitWebView *web_view = topbox_children->data;
			gtk_container_remove(GTK_CONTAINER(topbox), GTK_WIDGET(web_view));
		}
		gtk_container_remove(GTK_CONTAINER(mainbox), GTK_WIDGET(topbox));
	}

	GtkWidget *topbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(topbox), GTK_WIDGET(buffer->web_view), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainbox), GTK_WIDGET(topbox), TRUE, TRUE, 0);

	gtk_widget_grab_focus(GTK_WIDGET(buffer->web_view));
	// In case a link is opened externally in a new buffer (but not a new window),
	// we need to present Next to the user to make it obvious that the link was
	// opened.
	gtk_window_present(GTK_WINDOW(window->base));

	gtk_widget_show_all(window->base);
}

gint64 window_set_minibuffer_height(Window *window, gint64 height) {
	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));
	if (g_list_length(box_children) > 1) {
		box_children = box_children->next;
	}
	gtk_widget_set_size_request(GTK_WIDGET(box_children->data), -1, height);
	window->minibuffer_height = height;

	gint minimum_height;
	gint natural_height;
	gtk_widget_get_preferred_height(GTK_WIDGET(window->minibuffer->web_view), &minimum_height, &natural_height);
	g_debug("minimum height %i, natural_height %i", minimum_height, natural_height);
	return natural_height;
}

void window_set_title(Window *window, const char *title) {
	gtk_window_set_title(GTK_WINDOW(window->base), title);
}
