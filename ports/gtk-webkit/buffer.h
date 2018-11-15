/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>

typedef struct {
	WebKitWebView *web_view;
	int callback_count;
	char *identifier;
} Buffer;

void buffer_set_url(Buffer *buffer, char *url) {
	webkit_web_view_load_uri(buffer->web_view, url);
}

Buffer *buffer_init() {
	Buffer *buffer = calloc(1, sizeof (Buffer));
	buffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	buffer->callback_count = 0;
	buffer_set_url(buffer, "https://next.atlas.engineer/");
	return buffer;
}

void buffer_delete(Buffer *buffer) {
	gtk_widget_destroy(GTK_WIDGET(buffer->web_view));
}

static void buffer_javascript_callback(GObject *object, GAsyncResult *result,
	gpointer user_data) {
	WebKitJavascriptResult *js_result;
	JSValueRef value;
	JSGlobalContextRef context;
	GError *error = NULL;
	js_result = webkit_web_view_run_javascript_finish(WEBKIT_WEB_VIEW(object), result, &error);
	if (!js_result) {
		g_warning("Error running javascript: %s", error->message);
		g_error_free(error);
		return;
	}

	context = webkit_javascript_result_get_global_context(js_result);
	value = webkit_javascript_result_get_value(js_result);
	if (JSValueIsString(context, value)) {
		JSStringRef js_str_value;
		gchar *str_value;
		gsize str_length;

		js_str_value = JSValueToStringCopy(context, value, NULL);
		str_length = JSStringGetMaximumUTF8CStringSize(js_str_value);
		str_value = (gchar *)g_malloc(str_length);
		JSStringGetUTF8CString(js_str_value, str_value, str_length);
		JSStringRelease(js_str_value);
		*((char **)user_data) = str_value;
	} else {
		g_warning("Error running javascript: unexpected return value");
	}
	webkit_javascript_result_unref(js_result);
}

char *buffer_evaluate(Buffer *buffer, const char *javascript) {
	buffer->callback_count++;
	char *result = NULL;
	webkit_web_view_run_javascript(buffer->web_view, javascript,
		NULL, buffer_javascript_callback, &result);
	return result;
}
