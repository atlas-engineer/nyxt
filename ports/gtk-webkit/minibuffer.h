/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>

#include "javascript.h"

typedef struct {
	WebKitWebView *web_view;
	int callback_count;
	char *parent_window_identifier;
} Minibuffer;

typedef struct {
	Minibuffer *minibuffer;
	int callback_id;
} MinibufferInfo;

gboolean minibuffer_web_view_web_process_crashed(WebKitWebView *_web_view,
	Minibuffer *minibuffer) {
	g_warning("Window %s minibuffer web process crashed",
		minibuffer->parent_window_identifier);
	return FALSE;
}

Minibuffer *minibuffer_init() {
	Minibuffer *minibuffer = calloc(1, sizeof (Minibuffer));
	minibuffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	minibuffer->callback_count = 0;

	g_signal_connect(minibuffer->web_view, "web-process-crashed",
		G_CALLBACK(minibuffer_web_view_web_process_crashed), minibuffer);

	return minibuffer;
}

void minibuffer_delete(Minibuffer *minibuffer) {
	gtk_widget_destroy(GTK_WIDGET(minibuffer->web_view));
	g_free(minibuffer->parent_window_identifier);
	g_free(minibuffer);
}

static void minibuffer_javascript_callback(GObject *object, GAsyncResult *result,
	gpointer user_data) {
	MinibufferInfo *minibuffer_info = (MinibufferInfo *)user_data;
	javascript_transform_result(object, result, minibuffer_info->minibuffer->parent_window_identifier,
		minibuffer_info->callback_id);
	g_free(minibuffer_info);
}

// Caller must free the result.
char *minibuffer_evaluate(Minibuffer *minibuffer, const char *javascript) {
	// If another minibuffer_evaluate is run before the callback is called, there
	// will be a race condition upon accessing callback_count.
	// Thus we send a copy of callback_count via a MinibufferInfo to the callback.
	// The MinibufferInfo must be freed in the callback.
	MinibufferInfo *minibuffer_info = g_new(MinibufferInfo, 1);
	minibuffer_info->minibuffer = minibuffer;
	minibuffer_info->callback_id = minibuffer->callback_count;

	minibuffer->callback_count++;

	webkit_web_view_run_javascript(minibuffer->web_view, javascript,
		NULL, minibuffer_javascript_callback, minibuffer_info);
	return g_strdup_printf("%i", minibuffer_info->callback_id);
}
