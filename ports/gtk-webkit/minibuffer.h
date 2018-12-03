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
	char *parent_window_identifier;
} Minibuffer;

typedef struct {
	Minibuffer *minibuffer;
	int callback_id;
} MinibufferInfo;

Minibuffer *minibuffer_init() {
	Minibuffer *minibuffer = calloc(1, sizeof (Minibuffer));
	minibuffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	minibuffer->callback_count = 0;
	return minibuffer;
}

void minibuffer_delete(Minibuffer *minibuffer) {
	gtk_widget_destroy(GTK_WIDGET(minibuffer->web_view));
	g_free(minibuffer->parent_window_identifier);
	g_free(minibuffer);
}

// TODO: Factor minibuffer_javascript_callback and buffer_javascript_callback?
static void minibuffer_javascript_callback(GObject *object, GAsyncResult *result,
	gpointer user_data) {
	gchar *transformed_result = javascript_result(object, result, user_data);
	g_debug("Javascript result: %s", transformed_result);
	if (transformed_result == NULL) {
		return;
	}

	MinibufferInfo *minibuffer_info = (MinibufferInfo *)user_data;

	GError *error = NULL;
	const char *method_name = "MINIBUFFER-JAVASCRIPT-CALL-BACK";
	char *id = g_strdup_printf("%i", minibuffer_info->callback_id);
	GVariant *params = g_variant_new(
		"(sss)",
		minibuffer_info->minibuffer->parent_window_identifier,
		transformed_result,
		id);
	g_message("XML-RPC message: %s (minibuffer id, javascript, callback id) = (%s, ..., %s)",
		method_name,
		minibuffer_info->minibuffer->parent_window_identifier,
		id);
	g_debug("Javascript: %s", transformed_result);

	g_free(id);
	g_free(minibuffer_info);
	g_free(transformed_result);

	SoupMessage *msg = soup_xmlrpc_message_new(state.core_socket,
			method_name, params, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return;
	}

	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
}

char *minibuffer_evaluate(Minibuffer *minibuffer, const char *javascript) {
	minibuffer->callback_count++;

	// If another minibuffer_evaluate is run before the callback is called, there
	// will be a race condition upon accessing callback_count.
	// Thus we send a copy of callback_count via a BufferInfo to the callback.
	// The MinibufferInfo must be freed in the callback.
	MinibufferInfo *minibuffer_info = g_new(MinibufferInfo, 1);
	minibuffer_info->minibuffer = minibuffer;
	minibuffer_info->callback_id = minibuffer->callback_count;

	webkit_web_view_run_javascript(minibuffer->web_view, javascript,
		NULL, minibuffer_javascript_callback, minibuffer_info);
	g_debug("minibuffer_evaluate callback count: %i", minibuffer_info->callback_id);
	return g_strdup_printf("%i", minibuffer_info->callback_id);
}
