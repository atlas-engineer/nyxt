/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>

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
	gtk_widget_destroy(GTK_WIDGET(minibuffer->web_view));
}

static void minibuffer_javascript_callback(GObject *object, GAsyncResult *result,
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

char *minibuffer_evaluate(Minibuffer *minibuffer, const char *javascript) {
	minibuffer->callback_count++;
	char *result = NULL;
	webkit_web_view_run_javascript(minibuffer->web_view, javascript,
		NULL, minibuffer_javascript_callback, &result);
	// TODO: Call XML RPC with result.
	return result;
}
