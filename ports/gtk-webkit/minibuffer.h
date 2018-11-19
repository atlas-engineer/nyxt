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

Minibuffer *minibuffer_init() {
	Minibuffer *minibuffer = calloc(1, sizeof (Minibuffer));
	minibuffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	minibuffer->callback_count = 0;
	return minibuffer;
}

void minibuffer_delete(Minibuffer *minibuffer) {
	// TODO: Should we clean up the minibuffer?
}

static void minibuffer_javascript_callback(GObject *object, GAsyncResult *result,
	gpointer user_data) {
	gchar *transformed_result = javascript_result(object, result, user_data);
	g_debug("Javascript result: %s", transformed_result);
	if (transformed_result == NULL) {
		return;
	}

	Minibuffer *minibuffer = (Minibuffer *)user_data;

	GError *error = NULL;
	const char *method_name = "MINIBUFFER-JAVASCRIPT-CALL-BACK";
	GVariant *params = g_variant_new(
		"(sss)",
		minibuffer->parent_window_identifier,
		transformed_result,
		g_strdup_printf("%i", minibuffer->callback_count));
	g_debug("XML-RPC message: %s %s", method_name, g_variant_print(params, TRUE));

	SoupMessage *msg = soup_xmlrpc_message_new("http://localhost:8081/RPC2",
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
	webkit_web_view_run_javascript(minibuffer->web_view, javascript,
		NULL, minibuffer_javascript_callback, minibuffer);
	g_debug("minibuffer_evaluate callback count: %i", minibuffer->callback_count);
	return g_strdup_printf("%i", minibuffer->callback_count);
}
