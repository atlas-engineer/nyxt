/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>

// Return value must be freed.
gchar *javascript_result(GObject *object, GAsyncResult *result,
	gpointer user_data) {
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
