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
	const char *method_name = "BUFFER-DID-COMMIT-NAVIGATION";

	switch (load_event) {
	case WEBKIT_LOAD_STARTED:
		/* New load, we have now a provisional URI */
		/* Here we could start a spinner or update the
		 * location bar with the provisional URI */
		break;
	case WEBKIT_LOAD_REDIRECTED:
		// TODO: Let the core know that we have been redirected?
		break;
	case WEBKIT_LOAD_COMMITTED:
		/* The load is being performed. Current URI is
		 * the final one and it won't change unless a new
		 * load is requested or a navigation within the
		 * same page is performed */
		uri = webkit_web_view_get_uri(web_view);

		// TODO: Notify Lisp core on invalid TLS certificate, leave to the Lisp core
		// the possibility to load the non-HTTPS URL.
		if (g_str_has_prefix(uri, "https://")) {
			GTlsCertificateFlags tls_flags;
			if (webkit_web_view_get_tls_info(web_view, NULL, &tls_flags) && tls_flags) {
				g_warning("Invalid TLS certificate");
			}
		}

		break;
	case WEBKIT_LOAD_FINISHED:
		/* Load finished, we can now stop the spinner */
		method_name = "BUFFER-DID-FINISH-NAVIGATION";
	}

	if (uri == NULL) {
		return;
	}

	g_debug("Load changed: %s", uri);

	Buffer *buffer = data;
	GError *error = NULL;
	GVariant *arg = g_variant_new("(ss)", buffer->identifier, uri);
	g_message("XML-RPC message: %s %s", method_name, g_variant_print(arg, TRUE));

	SoupMessage *msg = soup_xmlrpc_message_new(state.core_socket,
			method_name, arg, &error);

	if (error) {
		g_warning("Malformed XML-RPC message: %s", error->message);
		g_error_free(error);
		return;
	}
	soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
	// 'msg' and 'uri' are freed automatically.
}

gboolean buffer_web_view_decide_policy(WebKitWebView *web_view,
	WebKitPolicyDecision *decision, WebKitPolicyDecisionType type, gpointer buffer) {
	switch (type) {
	case WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION:
		// TODO: Force-load the URL in a new buffer on a specific keybinding.
		// TODO: Handle special resources, like file:///...
		return FALSE;
	case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION: {
		WebKitNavigationAction *action;
		action = webkit_navigation_policy_decision_get_navigation_action(WEBKIT_NAVIGATION_POLICY_DECISION(decision));

		// Don't load if this was started without user interaction.
		if (!webkit_navigation_action_is_user_gesture(action)) {
			g_debug("Policy: New window: Ignore without user interaction");
			webkit_policy_decision_ignore(decision);
			return TRUE;
		}

		if (webkit_navigation_action_get_navigation_type(action) ==
			WEBKIT_NAVIGATION_TYPE_LINK_CLICKED) {
			g_debug("Policy: New window: Load in new buffer");
			webkit_policy_decision_ignore(decision);
			// New window can be triggered with the following HTML:
			// <a href="http://example.org" target="_blank">New window</a>
			GError *error = NULL;
			WebKitURIRequest *request = webkit_navigation_action_get_request(action);
			const char *method_name = "MAKE-BUFFERS";

			// Warning: we need to pass a list of URLs, even if we pass only one URL.
			GVariantBuilder builder;
			g_variant_builder_init(&builder, G_VARIANT_TYPE("as"));
			g_variant_builder_add(&builder, "s", webkit_uri_request_get_uri(request));

			GVariant *arg = g_variant_new("(as)", &builder);
			g_message("XML-RPC message: %s %s", method_name, g_variant_print(arg, TRUE));

			SoupMessage *msg = soup_xmlrpc_message_new(state.core_socket,
					method_name, arg, &error);

			if (error) {
				g_warning("Malformed XML-RPC message: %s", error->message);
				g_error_free(error);
				// TODO: Return TRUE or FALSE?
				return FALSE;
			}
			soup_session_queue_message(xmlrpc_env, msg, NULL, NULL);
			return TRUE;
		}
		return FALSE;
	}
	case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
		// Entry-point for downloads.
		if (!webkit_response_policy_decision_is_mime_type_supported(WEBKIT_RESPONSE_POLICY_DECISION(decision))) {
			g_debug("Policy: Response: Download");
			webkit_policy_decision_download(decision);
			return TRUE;
		}
		return FALSE;
	}

	return FALSE;
}

void buffer_set_cookie_file(Buffer *buffer, const char *path) {
	if (path == NULL) {
		return;
	}
	WebKitCookieManager *cookie_manager =
		webkit_web_context_get_cookie_manager(webkit_web_view_get_context(buffer->web_view));
	webkit_cookie_manager_set_persistent_storage(cookie_manager,
		path,
	        // TODO: Make format configurable?
		WEBKIT_COOKIE_PERSISTENT_STORAGE_TEXT);
}

void buffer_web_view_download_started(WebKitWebContext *context,
	WebKitDownload *download, Buffer *buffer) {
	const char *uri = webkit_uri_request_get_uri(webkit_download_get_request(download));
	g_debug("Download starting: %s", uri);
	/*
	Signal handlers accessible from here:
	        - decide-destination
	        - failed
	        - finished
	        - received-data
	*/
}

Buffer *buffer_init(const char *cookie_file) {
	Buffer *buffer = calloc(1, sizeof (Buffer));
	buffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	buffer_set_cookie_file(buffer, cookie_file);

	g_signal_connect(buffer->web_view, "load-changed", G_CALLBACK(buffer_web_view_load_changed), buffer);
	g_signal_connect(buffer->web_view, "decide-policy", G_CALLBACK(buffer_web_view_decide_policy), buffer);


	WebKitWebContext *context = webkit_web_view_get_context(buffer->web_view);
	g_signal_connect(context, "download-started", G_CALLBACK(buffer_web_view_download_started), buffer);

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
	BufferInfo *buffer_info = (BufferInfo *)user_data;
	javascript_transform_result(object, result, buffer_info->buffer->identifier,
		buffer_info->callback_id);
	g_free(buffer_info);
}

// Caller must free the result.
char *buffer_evaluate(Buffer *buffer, const char *javascript) {
	// If another buffer_evaluate is run before the callback is called, there will
	// be a race condition upon accessing callback_count.
	// Thus we send a copy of callback_count via a BufferInfo to the callback.
	// The BufferInfo must be freed in the callback.
	BufferInfo *buffer_info = g_new(BufferInfo, 1);
	buffer_info->buffer = buffer;
	buffer_info->callback_id = buffer->callback_count;

	buffer->callback_count++;

	webkit_web_view_run_javascript(buffer->web_view, javascript,
		NULL, buffer_javascript_callback, buffer_info);
	g_debug("buffer_evaluate callback count: %i", buffer_info->callback_id);
	return g_strdup_printf("%i", buffer_info->callback_id);
}
