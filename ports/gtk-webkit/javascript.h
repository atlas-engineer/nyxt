/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>
#include "client.h"
#include "server-state.h"

// Return value must be freed.
gchar *javascript_result(GObject *object, GAsyncResult *result,
	gpointer _data) {
	WebKitJavascriptResult *js_result;
	JSValueRef value;
	JSGlobalContextRef context;
	GError *error = NULL;
	js_result = webkit_web_view_run_javascript_finish(WEBKIT_WEB_VIEW(object), result, &error);
	if (!js_result) {
		g_warning("Error running javascript: %s", error->message);
		g_error_free(error);
		return NULL;
	}

	// TODO: Deprecated, use jsc_value_get_context instead.
	context = webkit_javascript_result_get_global_context(js_result);
	value = webkit_javascript_result_get_value(js_result);
	if (!JSValueIsString(context, value)) {
		g_warning("Error running javascript: unexpected return value");
		webkit_javascript_result_unref(js_result);
		return NULL;
	}

	JSStringRef js_str_value;
	gchar *str_value;
	gsize str_length;

	js_str_value = JSValueToStringCopy(context, value, NULL);
	str_length = JSStringGetMaximumUTF8CStringSize(js_str_value);
	str_value = (gchar *)g_malloc(str_length);
	JSStringGetUTF8CString(js_str_value, str_value, str_length);
	JSStringRelease(js_str_value);

	return str_value;
}

void javascript_transform_result(GObject *object, GAsyncResult *result,
	const char *identifier, int callback_id) {
	gchar *transformed_result = javascript_result(object, result, NULL);
	g_debug("Javascript result: %s", transformed_result);
	if (transformed_result == NULL) {
		return;
	}

	GError *error = NULL;
	const char *method_name = "BUFFER-JAVASCRIPT-CALL-BACK";
	char *callback_string = g_strdup_printf("%i", callback_id);
	GVariant *params = g_variant_new(
		"(sss)",
		identifier,
		transformed_result,
		callback_string);
	g_message("XML-RPC message: %s (buffer id, javascript, callback id) = (%s, ..., %s)",
		method_name,
		identifier,
		callback_string);
	g_debug("Javascript: %s", transformed_result);

	g_free(callback_string);
	g_free(transformed_result);

	// 'params' is floating and soup_xmlrpc_message_new will consume it.
	SoupMessage *msg = soup_xmlrpc_message_new(state.core_socket,
			method_name, params, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return;
	}

	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
}
