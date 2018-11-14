/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <stdio.h>

#include <glib.h>

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#include <libsoup/soup.h>

#define APPNAME "Next"

static void destroy_window(GtkWidget *widget, GtkWidget *window) {
	gtk_main_quit();
}

static gboolean close_web_view(WebKitWebView *webView, GtkWidget *window) {
	gtk_widget_destroy(window);
	return true;
}

static char *window_make_gui() {
	// Create an 800x600 window that will contain the browser instance
	GtkWidget *main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	gtk_window_set_default_size(GTK_WINDOW(main_window), 800, 600);
	// TODO: Make title customizable from Lisp.
	gtk_window_set_title(GTK_WINDOW(main_window), APPNAME);
	// TODO: Deprecated?
	/* gtk_window_set_wmclass(GTK_WINDOW(main_window), APPNAME, APPNAME); */

	// Create a browser instance
	WebKitWebView *web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());

	// Put the browser area into the main window
	gtk_container_add(GTK_CONTAINER(main_window), GTK_WIDGET(web_view));

	// Set up callbacks so that if either the main window or the browser
	// instance is closed, the program will exit
	g_signal_connect(main_window, "destroy", G_CALLBACK(destroy_window), NULL);
	g_signal_connect(web_view, "close", G_CALLBACK(close_web_view), main_window);

	// Load a web page into the browser instance
	webkit_web_view_load_uri(web_view, "https://next.atlas.engineer/");

	// Make sure that when the browser area becomes visible, it will get
	// mouse and keyboard events
	gtk_widget_grab_focus(GTK_WIDGET(web_view));

	// Make sure the main window and all its contents are visible
	gtk_widget_show_all(main_window);

	// TODO: Return the hash table key.
	return "foo";
}

static void server_callback(SoupServer *server, SoupMessage *msg,
	const char *path, GHashTable *query,
	SoupClientContext *context, gpointer data) {
	SoupMessageHeadersIter iter;
	const char *name, *value;

	g_debug("%s %s HTTP/1.%d", msg->method, path,
		soup_message_get_http_version(msg));
	soup_message_headers_iter_init(&iter, msg->request_headers);
	while (soup_message_headers_iter_next(&iter, &name, &value)) {
		g_debug("%s: %s", name, value);
	}

	if (msg->request_body->length == 0) {
		g_warning("Empty HTTP request");
		return;
	}

	g_debug("%s", msg->request_body->data);
	SoupXMLRPCParams *params = NULL;
	GError *error = NULL;
	char *method_name = soup_xmlrpc_parse_request(msg->request_body->data,
			msg->request_body->length,
			&params,
			&error);
	if (error) {
		g_warning("Malformed XMLRPC request: %s", error->message);
		return;
	}

	g_debug("Method name: %s", method_name);

	if (strcmp(method_name, "window.make") == 0) {
		window_make_gui();
	}

	/*
	GVariant *variant = soup_xmlrpc_params_parse(params, NULL, &error);
	if (error) {
	        g_warning("Malformed method parameters: %s\n", error->message);
	}
	*/

	soup_xmlrpc_params_free(params);

	soup_xmlrpc_message_set_response(msg, g_variant_new_string("foo"), &error);
	if (error) {
		g_warning("Failed to set XMLRPC response: %s", error->message);
	}

	g_debug("Response: %d %s", msg->status_code, msg->reason_phrase);
}

int main(int argc, char *argv[]) {
	// TODO: Use GtkApplication?
	gtk_init(&argc, &argv);

	// TODO: Start the xmlrpc server first?  If GUI is started, then we can
	// report xmlrpc startup issue graphically.
	// TODO: Server logging?
	// TODO: libsoup's examples don't unref the server.  Should we?
	SoupServer *server = soup_server_new(
		/* SOUP_SERVER_SERVER_HEADER, APPNAME, */
		NULL);

	GError *error = NULL;
	soup_server_listen_all(server, 8082, 0, &error);
	if (error) {
		g_printerr("Unable to create server: %s\n", error->message);
		exit(1);
	}
	g_debug("Starting XMLRPC server");
	soup_server_add_handler(server, NULL, server_callback, NULL, NULL);

	gtk_main();

	return EXIT_SUCCESS;
}
