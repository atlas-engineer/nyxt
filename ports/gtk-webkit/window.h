/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#define APPNAME "Next"

#include <webkit2/webkit2.h>
#include "buffer.h"
#include "minibuffer.h"

typedef struct {
	GtkWidget *base;
	Buffer *buffer;
	const char *identifier;
	Minibuffer *minibuffer;
	int minibuffer_height;
} Window;

static void destroy_window(GtkWidget *widget, GtkWidget *window) {
	// TODO: Call only on last window.
	gtk_main_quit();
}

static gboolean close_web_view(WebKitWebView *webView, GtkWidget *window) {
	// TODO: Call window_delete with identifier.
	gtk_widget_destroy(window);
	return true;
}

Window *window_init() {
	Buffer *buffer = buffer_init();
	// Make sure that when the browser area becomes visible, it will get
	// mouse and keyboard events
	gtk_widget_grab_focus(GTK_WIDGET(buffer->web_view));

	Minibuffer *minibuffer = minibuffer_init();

	GtkWidget *mainbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_end(GTK_BOX(mainbox), GTK_WIDGET(buffer->web_view), TRUE, TRUE, 0);
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
	// instance is closed, the program will exit
	g_signal_connect(window->base, "destroy", G_CALLBACK(destroy_window), NULL);
	g_signal_connect(buffer->web_view, "close", G_CALLBACK(close_web_view), window->base);

	// Make sure the main window and all its contents are visible
	gtk_widget_show_all(window->base);

	window->buffer = buffer;
	window->minibuffer = minibuffer;
	return window;
}

void window_delete(Window *window) {
	gtk_widget_destroy(window->base);
	// TODO: Kill buffer?
}

void window_set_active_buffer(Window *window, Buffer *buffer) {
	window->buffer = buffer;
	gtk_container_add(GTK_CONTAINER(window->base), GTK_WIDGET(buffer->web_view));
}

gint64 window_set_minibuffer_height(Window *window, gint64 height) {
	gtk_widget_set_size_request(GTK_WIDGET(window->minibuffer), -1, height);
	window->minibuffer_height = height;

	gint natural_height;
	gtk_widget_get_preferred_height(GTK_WIDGET(window->minibuffer), NULL, &natural_height);
	return natural_height;
}
