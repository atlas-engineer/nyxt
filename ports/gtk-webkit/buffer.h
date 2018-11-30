/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>

#include "javascript.h"
#include "client.h"

typedef struct {
	WebKitWebView *web_view;
	int callback_count;
	char *identifier;
} Buffer;

typedef struct {
	Buffer *buffer;
	int callback_id;
} BufferInfo;


static void buffer_web_view_load_changed(WebKitWebView *web_view,
	WebKitLoadEvent load_event,
	gpointer data) {
	const char *uri = NULL;
	switch (load_event) {
	case WEBKIT_LOAD_STARTED:
		/* New load, we have now a provisional URI */
		uri = webkit_web_view_get_uri(web_view);
		/* Here we could start a spinner or update the
		 * location bar with the provisional URI */
		break;
	case WEBKIT_LOAD_REDIRECTED:
		// TODO: Let the core know that we have been redirected?
		uri = webkit_web_view_get_uri(web_view);
		break;
	case WEBKIT_LOAD_COMMITTED:
		/* The load is being performed. Current URI is
		 * the final one and it won't change unless a new
		 * load is requested or a navigation within the
		 * same page is performed */
		uri = webkit_web_view_get_uri(web_view);
		break;
	case WEBKIT_LOAD_FINISHED:
		/* Load finished, we can now stop the spinner */
		return;
	}
	g_debug("Load changed: %s", uri);

	Buffer *buffer = data;
	GError *error = NULL;
	const char *method_name = "BUFFER-DID-COMMIT-NAVIGATION";
	GVariant *buffer_id = g_variant_new("(s)", buffer->identifier);
	GVariant *uri_variant = g_variant_new("(s)", uri);
	g_debug("XML-RPC message: %s %s", method_name, g_variant_print(uri_variant, TRUE));

	SoupMessage *msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
			method_name, uri_variant, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return;
	}
	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
	// TODO: Free message?
	/* g_free(msg); */

	// TODO: Free URI string?
	/* g_free(uri); */
}

void buffer_set_url(Buffer *buffer, const char *url) {
	webkit_web_view_load_uri(buffer->web_view, url);
}

Buffer *buffer_init() {
	Buffer *buffer = calloc(1, sizeof (Buffer));
	buffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	g_signal_connect(buffer->web_view, "load-changed", G_CALLBACK(buffer_web_view_load_changed), buffer);
	// We need to hold a reference to the view, otherwise changing buffer in the a
	// window will unref+destroy the view.
	g_object_ref(buffer->web_view);
	g_debug("Init buffer %p with view %p", buffer, buffer->web_view);
	buffer->callback_count = 0;
	// So far we leave the core to set the default URL, otherwise the load-changed
	// signal would be emitted while the buffer identifier is still empty.
	return buffer;
}

void buffer_delete(Buffer *buffer) {
	// Remove the extra ref added in buffer_init()?
	/* g_object_unref(buffer->web_view); */
	// TODO: What happens to the Window's web view when current buffer is deleted?

	gtk_widget_destroy(GTK_WIDGET(buffer->web_view));
	g_free(buffer->identifier);
	g_free(buffer);
}

static void buffer_javascript_callback(GObject *object, GAsyncResult *result,
	gpointer user_data) {
	gchar *transformed_result = javascript_result(object, result, user_data);
	g_debug("Javascript result: %s", transformed_result);
	if (transformed_result == NULL) {
		return;
	}

	BufferInfo *buffer_info = (BufferInfo *)user_data;

	GError *error = NULL;
	const char *method_name = "BUFFER-JAVASCRIPT-CALL-BACK";
	// TODO: Make floating params so that they are consumed in soup_xmlrpc_message_new?
	GVariant *params = g_variant_new(
		"(sss)",
		buffer_info->buffer->identifier,
		transformed_result,
		// TODO: Free this:
		g_strdup_printf("%i", buffer_info->callback_id));
	g_debug("XML-RPC message: %s %s", method_name, g_variant_print(params, TRUE));

	g_free(buffer_info);
	g_free(transformed_result);

	SoupMessage *msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
			method_name, params, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return;
	}

	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
}

char *buffer_evaluate(Buffer *buffer, const char *javascript) {
	buffer->callback_count++;

	// If another buffer_evaluate is run before the callback is called, there will
	// be a race condition upon accessing callback_count.
	// Thus we send a copy of callback_count via a BufferInfo to the callback.
	// The BufferInfo must be freed in the callback.
	BufferInfo *buffer_info = g_new(BufferInfo, 1);
	buffer_info->buffer = buffer;
	buffer_info->callback_id = buffer->callback_count;

	webkit_web_view_run_javascript(buffer->web_view, javascript,
		NULL, buffer_javascript_callback, buffer_info);
	g_debug("buffer_evaluate callback count: %i", buffer_info->callback_id);
	return g_strdup_printf("%i", buffer_info->callback_id);
}
