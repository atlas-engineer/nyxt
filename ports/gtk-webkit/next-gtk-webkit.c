/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <stdio.h>

#include <glib.h>

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "message-queue.h"

#define APPNAME "Next"

// TODO: Make local.
GAsyncQueue *main_queue;

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
	gtk_window_set_title(GTK_WINDOW(main_window), APPNAME);

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

static xmlrpc_value *window_make_request(xmlrpc_env *const envP,
	xmlrpc_value *const paramArrayP, void *const serverInfo,
	void *const channelInfo) {
	g_async_queue_push(main_queue, &window_make_gui);
	// TODO: Need to trigger Glib to process the async queue, otherwise it will
	// wait until next keyboard event, timeout, etc.

	// TODO: Get operation result.
	return xmlrpc_build_value(envP, "s", "result");
}

static void server_start() {
	struct xmlrpc_method_info3 const window_make = {
		"window.make", &window_make_request
	};
	/*
	struct xmlrpc_method_info3 const windowDelete = {
	"window.delete", &window_delete,};
	struct xmlrpc_method_info3 const windowActive = {
	"window.active", &window_active,};
	struct xmlrpc_method_info3 const windowSetActiveBuffer = {
	"window.set.active.buffer", &window_set_active_buffer,};
	struct xmlrpc_method_info3 const bufferMake = {
	"buffer.make", &buffer_make,};
	struct xmlrpc_method_info3 const bufferExecuteJavaScript = {
	"buffer.execute.javascript", &buffer_execute_javascript,};
	struct xmlrpc_method_info3 const minibufferSetHeight = {
	"minibuffer.set.height", &minibuffer_set_height,};
	struct xmlrpc_method_info3 const minibufferExecuteJavascript = {
	"minibuffer.execute.javascript", &minibuffer_execute_javascript,};
	*/

	xmlrpc_server_abyss_parms serverparm;
	xmlrpc_registry *registryP;
	xmlrpc_env env;
	xmlrpc_env_init(&env);

	registryP = xmlrpc_registry_new(&env);
	if (env.fault_occurred) {
		printf("xmlrpc_registry_new() failed: %s\n", env.fault_string);
		exit(1);
	}

	xmlrpc_registry_add_method3(&env, registryP, &window_make);
	/*
	xmlrpc_registry_add_method3(&env, registryP, &windowDelete);
	xmlrpc_registry_add_method3(&env, registryP, &windowActive);
	xmlrpc_registry_add_method3(&env, registryP, &windowSetActiveBuffer);
	xmlrpc_registry_add_method3(&env, registryP, &bufferMake);
	xmlrpc_registry_add_method3(&env, registryP,
	&bufferExecuteJavaScript);
	xmlrpc_registry_add_method3(&env, registryP, &minibufferSetHeight);
	xmlrpc_registry_add_method3(&env, registryP,
	&minibufferExecuteJavascript);
	*/

	if (env.fault_occurred) {
		printf("xmlrpc_registry_add_method3() failed.  %s\n",
			env.fault_string);
		exit(1);
	}

	serverparm.config_file_name = NULL; /* Select the modern normal API */
	serverparm.registryP = registryP;
	serverparm.port_number = 8082;
	// TODO: Use .log extension?
	serverparm.log_file_name = "/tmp/next_xmlrpc_log";

	g_message("Starting xmlrpc server");
	xmlrpc_server_abyss(&env, &serverparm, XMLRPC_APSIZE(log_file_name));
	g_message("Stopping xmlrpc server");
	if (env.fault_occurred) {
		printf("xmlrpc_server_abyss() failed.  %s\n", env.fault_string);
		exit(1);
	}
}

typedef char * (*GuiCallback) ();
gboolean main_queue_callback(gpointer message, gpointer user_data) {
	g_message("Callback");
	GuiCallback callback = (GuiCallback)message;
	// TODO: Get return value.
	callback();

	// Return TRUE to keep the GSource alive.
	return TRUE;
}

int main(int argc, char *argv[]) {
	// TODO: Use GtkApplication?
	gtk_init(&argc, &argv);

	// TODO: Start the xmlrpc server first?  If GUI is started, then we can
	// report xmlrpc startup issue graphically.
	GThread *server_thread = g_thread_new("server", (GThreadFunc) &server_start, NULL);

	main_queue = g_async_queue_new();
	GSource *message_queue_source = message_queue_source_new(main_queue, NULL, NULL);
	g_source_set_callback(message_queue_source, (GSourceFunc) &main_queue_callback, NULL, NULL);
	g_source_attach(message_queue_source, NULL);
	gtk_main();

	// TODO: Kill xmlrpc server cleanly.
	// We can use xmlrpc_server_abyss_terminate:
	// http://xmlrpc-c.sourceforge.net/doc/libxmlrpc_server_abyss.html#server_abyss_terminate
	g_thread_join(server_thread);

	return EXIT_SUCCESS;
}
