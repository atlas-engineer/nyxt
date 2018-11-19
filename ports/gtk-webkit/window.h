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

void window_event_callback(SoupSession *_session, SoupMessage *msg, gpointer _data) {
	GError *error = NULL;
	g_debug("XML-RPC response: %s", msg->response_body->data);
	GVariant *consumed = soup_xmlrpc_parse_response(msg->response_body->data,
			msg->response_body->length, "b", &error);

	if (error) {
		g_warning("Malformed XML-RPC response: %s", error->message);
		g_error_free(error);
		return;
	}

	if (!g_variant_get_boolean(consumed)) {
		// TODO: If not consumed, forward to GTK.
		g_debug("Event not consumed, forwarding to GTK");
	}
}

void window_send_event(GtkWidget *_widget, GdkEventKey *event, gpointer _data) {
	g_debug("Key press value: %i", (gint32)event->keyval);

	const char *method_name = "PUSH-KEY-CHORD";

	GError *error = NULL;
	gboolean control_pressed = event->state & GDK_CONTROL_MASK;
	gboolean alt_pressed = event->state & GDK_MOD1_MASK;
	gboolean super_pressed = event->state & GDK_SUPER_MASK;
	// TODO: Send both hardware_keycode and keyval?
	GVariant *key_chord = g_variant_new("(bbbi)",
			control_pressed,
			alt_pressed,
			super_pressed,
			(gint32)event->keyval);
	g_debug("XML-RPC message: %s %s", method_name, g_variant_print(key_chord, TRUE));

	SoupMessage *msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
			method_name, key_chord, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return;
	}

	soup_session_queue_message(xmlrpc_env, msg, &window_event_callback, NULL);
}

Window *window_init() {
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
	// TODO: Connect to mainbox or window?
	// TODO: send event on press and/or release?
	g_signal_connect(mainbox, "key-press-event", G_CALLBACK(window_send_event), NULL);
	/* g_signal_connect(mainbox, "key-release-event", G_CALLBACK(window_send_event), NULL); */

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

	// Make sure the main window and all its contents are visible
	gtk_widget_show_all(window->base);

	window->buffer = buffer;
	window->minibuffer = minibuffer;
	return window;
}

void window_set_active_buffer(Window *window, Buffer *buffer) {
	window->buffer = buffer;
	g_debug("New active buffer %p with view %p", buffer, buffer->web_view);
	GList *children = gtk_container_get_children(GTK_CONTAINER(window->base));
	GtkWidget *mainbox = GTK_WIDGET(children->data);
	GList *box_children = gtk_container_get_children(GTK_CONTAINER(mainbox));
	g_debug("Remove buffer view %p from window", box_children->data);
	gtk_container_remove(GTK_CONTAINER(mainbox), GTK_WIDGET(box_children->data));

	gtk_box_pack_start(GTK_BOX(mainbox), GTK_WIDGET(buffer->web_view), TRUE, TRUE, 0);

	gtk_widget_show_all(window->base);
}

gint64 window_set_minibuffer_height(Window *window, gint64 height) {
	// TODO: Size request must be done on a widget, not a web view.
	gtk_widget_set_size_request(GTK_WIDGET(window->minibuffer), -1, height);
	window->minibuffer_height = height;

	gint natural_height;
	gtk_widget_get_preferred_height(GTK_WIDGET(window->minibuffer), NULL, &natural_height);
	return natural_height;
}
