/*
Copyright © 2018-2019 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <webkit2/webkit2.h>
#include <libsoup/soup.h>
#include <JavaScriptCore/JavaScript.h>

#include "javascript.h"

typedef struct {
	int mod;
	char *name;
} Modifier;

// See the documentation of "enum GdkModifierType".
static Modifier modifier_names[] = {
	{.mod = GDK_SHIFT_MASK, .name = "s"},
	{.mod = GDK_LOCK_MASK, .name = "Lock"},
	{.mod = GDK_CONTROL_MASK, .name = "C"},
	{.mod = GDK_MOD1_MASK, .name = "M"}, // Usually "Alt".
	// M is sometimes also seen as M2-M4, which can confuse the Lisp side.
	// I'm not sure that we need M2-M4 at all.
	/* {.mod = GDK_MOD2_MASK, .name = "M2"}, */
	/* {.mod = GDK_MOD3_MASK, .name = "M3"}, */
	/* {.mod = GDK_MOD4_MASK, .name = "M4"}, */
	{.mod = GDK_SUPER_MASK, .name = "S"},
	{.mod = GDK_HYPER_MASK, .name = "H"},
	{.mod = GDK_META_MASK, .name = "Meta"},
};

typedef struct {
	WebKitWebView *web_view;
	int callback_count;
	char *identifier;
	// WebKit does not seem to expose any accessor to the proxy settings, so we
	// need to store them ourselves.
	WebKitNetworkProxyMode _proxy_mode;
	const char *_proxy_uri;
	const char *const *_ignore_hosts;
	const char *const *_certificate_whitelist_hosts;
} Buffer;

typedef struct {
	Buffer *buffer;
	int callback_id;
} BufferInfo;


static void buffer_web_view_load_changed(WebKitWebView *web_view,
	WebKitLoadEvent load_event,
	gpointer data) {
	const char *uri = webkit_web_view_get_uri(web_view);
	const char *method_name = "buffer_did_commit_navigation";

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
		uri = webkit_web_view_get_uri(web_view); // TODO: Only need to set URI at the beginning?

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
		method_name = "buffer_did_finish_navigation";
		// TODO: This duplicates the buffer_did_commit_navigation call with
		// buffer_notify_uri.  But is it always the same URI?  What about
		// redirections?
		break;
	}

	if (uri == NULL) {
		return;
	}

	g_debug("Load changed: %s", uri);

	Buffer *buffer = data;
	GVariant *arg = g_variant_new("(ss)", buffer->identifier, uri);
	g_message("RPC message: %s %s", method_name, g_variant_print(arg, TRUE));

	g_dbus_connection_call(state.connection,
		CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
		method_name,
		arg,
		NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL, NULL);
	// 'msg' and 'uri' are freed automatically.
}

static void buffer_notify_uri(WebKitWebView *web_view, GParamSpec *_spec, gpointer data) {
	Buffer *buffer = (Buffer *)data;

	const char *method_name = "buffer_did_finish_navigation";
	const gchar *uri = webkit_web_view_get_uri(web_view);
	GVariant *arg = g_variant_new("(ss)", buffer->identifier, uri);
	g_message("RPC message: %s %s", method_name, g_variant_print(arg, TRUE));

	g_dbus_connection_call(state.connection,
		CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
		method_name,
		arg,
		NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL, NULL);
}

typedef struct  {
	WebKitPolicyDecision *decision;
	const gchar *uri;
} DecisionInfo;

void buffer_navigated_callback(GObject *_source, GAsyncResult *res, gpointer data) {
	GError *error = NULL;

	GVariant *loadv = g_dbus_connection_call_finish(state.connection,
			res,
			&error);

	if (error) {
		g_warning("%s", error->message);
		g_error_free(error);
		return;
	}

	g_debug("Buffer navigation RPC response: %s", g_variant_print(loadv, TRUE));
	gboolean load;
	g_variant_get(loadv, "(b)", &load);
	g_variant_unref(loadv);

	DecisionInfo *decision_info = data;
	WebKitPolicyDecision *decision = decision_info->decision;
	if (load) {
		// TODO: If we are told to load but this is a file to download, we should ignore it.
		g_debug("Load resource '%s'", decision_info->uri);
		webkit_policy_decision_use(decision);
	} else {
		g_debug("Ignore resource '%s'", decision_info->uri);
		webkit_policy_decision_ignore(decision);
	}

	g_object_unref(decision_info->decision);
	g_free(decision_info);
	return;
}

void buffer_set_uri(GObject *cookie_manager, GAsyncResult *res, gpointer data) {
	GError *error = NULL;
	char *cookies_string = "";
	{
		GList *cookies = webkit_cookie_manager_get_cookies_finish(
			(WebKitCookieManager *)cookie_manager, res, &error);

		if (error != NULL) {
			g_warning("%s", error->message);
			g_error_free(error);
			return;
		}

		GSList *scookies = NULL;
		while (cookies != NULL) {
			scookies = g_slist_prepend(scookies, cookies->data);
			cookies = cookies->next;
		}
		scookies = g_slist_reverse(scookies);
		if (scookies != NULL) {
			cookies_string = soup_cookies_to_cookie_header(scookies);
		}

		g_slist_free(scookies);
		g_list_free_full(cookies, (GDestroyNotify)soup_cookie_free);
	}

	GHashTable *args = data;

	char *buffer_id = g_hash_table_lookup(args, "buffer_id");
	char *uri = g_hash_table_lookup(args, "uri");
	WebKitPolicyDecision *decision = g_hash_table_lookup(args, "decision");
	char *event_type = g_hash_table_lookup(args, "event_type");
	gboolean *is_new_window = g_hash_table_lookup(args, "is_new_window");
	gboolean *is_known_type = g_hash_table_lookup(args, "is_known_type");
	char *mouse_button = g_hash_table_lookup(args, "mouse_button");
	guint *modifiers = g_hash_table_lookup(args, "modifiers");

	// Modifiers.
	GVariantBuilder builder;
	g_variant_builder_init(&builder, G_VARIANT_TYPE("as"));
	for (int i = 0; i < (sizeof modifier_names)/(sizeof modifier_names[0]); i++) {
		if (*modifiers & modifier_names[i].mod) {
			g_variant_builder_add(&builder, "s", modifier_names[i].name);
		}
	}

	const char *method_name = "request_resource";
	// TODO: Test if it's a redirect?
	GVariant *arg = g_variant_new("(ssssbbsas)", buffer_id, uri,
			cookies_string,
			event_type,
			*is_new_window,
			*is_known_type,
			mouse_button,
			&builder);
	g_message("RPC message: %s (buffer id, URI, cookies, event_type, is_new_window, is_known_type, button, modifiers) = %s",
		method_name, g_variant_print(arg, TRUE));

	if (g_strcmp0(mouse_button, "") != 0) {
		g_free(mouse_button);
	}
	if (g_strcmp0(cookies_string, "") != 0) {
		g_free(cookies_string);
	}
	g_free(is_new_window);
	g_free(is_known_type);
	g_free(modifiers);

	DecisionInfo *decision_info = g_new(DecisionInfo, 1);
	decision_info->decision = decision;
	decision_info->uri = uri;

	g_dbus_connection_call(state.connection,
		CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
		method_name,
		arg,
		NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL,
		(GAsyncReadyCallback)buffer_navigated_callback, decision_info);
}

gboolean buffer_web_view_decide_policy(WebKitWebView *web_view,
	WebKitPolicyDecision *decision, WebKitPolicyDecisionType type, gpointer bufferp) {
	WebKitNavigationAction *action = NULL;

	gboolean *is_new_window = g_new(gboolean, 1);
	*is_new_window = false;
	gboolean *is_known_type = g_new(gboolean, 1);
	*is_known_type = true;
	switch (type) {
	case WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION: {
		action = webkit_navigation_policy_decision_get_navigation_action(WEBKIT_NAVIGATION_POLICY_DECISION(decision));
		break;
	}
	case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION: {
		*is_new_window = true;
		action = webkit_navigation_policy_decision_get_navigation_action(WEBKIT_NAVIGATION_POLICY_DECISION(decision));
		break;
	}
	case WEBKIT_POLICY_DECISION_TYPE_RESPONSE: {
		if (!webkit_response_policy_decision_is_mime_type_supported(WEBKIT_RESPONSE_POLICY_DECISION(decision))) {
			*is_known_type = false;
		}
		break;
	}
	}

	WebKitURIRequest *request;
	if (action) {
		request = webkit_navigation_action_get_request(action);
	} else {
		request = webkit_response_policy_decision_get_request(WEBKIT_RESPONSE_POLICY_DECISION(decision));
	}
	const gchar *uri = webkit_uri_request_get_uri(request);

	gchar *event_type = "other";
	if (action) {
		switch (webkit_navigation_action_get_navigation_type(action)) {
		case WEBKIT_NAVIGATION_TYPE_LINK_CLICKED: {
			event_type = "link-click";
			break;
		}
		case WEBKIT_NAVIGATION_TYPE_FORM_SUBMITTED: {
			event_type = "form-submission";
			break;
		}
		case WEBKIT_NAVIGATION_TYPE_BACK_FORWARD: {
			event_type = "backward-or-forward";
			break;
		}
		case WEBKIT_NAVIGATION_TYPE_RELOAD: {
			event_type = "reload";
			break;
		}
		case WEBKIT_NAVIGATION_TYPE_FORM_RESUBMITTED: {
			event_type = "form-resubmission";
			break;
		}
		case WEBKIT_NAVIGATION_TYPE_OTHER: {
			break;
		}
		}
	}

	// No need for webkit_navigation_action_is_user_gesture if mouse_button and
	// modifiers tell us the same information.
	gchar *mouse_button = "";
	guint *modifiers = g_new(guint, 1);
	*modifiers = 0;
	if (action) {
		guint button = webkit_navigation_action_get_mouse_button(action);
		mouse_button = g_strdup_printf("button%d", button);

		*modifiers = webkit_navigation_action_get_modifiers(action);
	}

	WebKitCookieManager *cookie_manager =
		webkit_web_context_get_cookie_manager(webkit_web_view_get_context(web_view));

	Buffer *buffer = bufferp;
	GHashTable *args = g_hash_table_new(g_str_hash, g_str_equal);
	g_hash_table_insert(args, "decision", decision);
	g_hash_table_insert(args, "buffer_id", buffer->identifier);
	g_hash_table_insert(args, "uri", (char *)uri);
	g_hash_table_insert(args, "event_type", event_type);
	g_hash_table_insert(args, "is_new_window", is_new_window);
	g_hash_table_insert(args, "is_known_type", is_known_type);
	g_hash_table_insert(args, "mouse_button", mouse_button);
	g_hash_table_insert(args, "modifiers", modifiers);

	webkit_cookie_manager_get_cookies(cookie_manager,
		uri,
		NULL,
		(GAsyncReadyCallback)buffer_set_uri, args);

	// Keep a reference on the decision so that it won't be freed before the callback.
	g_object_ref(decision);
	return TRUE;
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

void buffer_web_view_download_started(WebKitWebContext *_context,
	WebKitDownload *download, Buffer *_buffer) {
	const char *uri = webkit_uri_request_get_uri(webkit_download_get_request(download));
	g_warning("Downloading %s because interception failed", uri);
}

gboolean buffer_web_view_web_process_crashed(WebKitWebView *_web_view, Buffer *buffer) {
	g_warning("Buffer %s web process crashed", buffer->identifier);
	return FALSE;
}

// Forward declarations because input events need to know about windows.
gboolean window_button_event(GtkWidget *_widget, GdkEventButton *event, gpointer window_data);
gboolean window_scroll_event(GtkWidget *_widget, GdkEventScroll *event, gpointer buffer_data);

static gboolean mouse_target_last_was_empty = true;
static gint64 mouse_target_last_changed = 0;
void buffer_mouse_target_changed(WebKitWebView *web_view,
	WebKitHitTestResult *result, guint _modifiers, gpointer _data) {
	const char *method_name = "buffer_uri_at_point";
	const gchar *uri = "";

	// When hovering fast over lots of links, we don't want to spam the Lisp core.
	// So we only send if the next event happend more than N milliseconds after the last.
	// TODO: The right way to do this is with a queue that gets processed every X
	// cycle: we discard messages in the queue that are too close, but we always
	// send the last one.
	gint64 current_time = g_get_monotonic_time();
	if ((current_time - mouse_target_last_changed) < (50*1000)) {
		return;
	}
	mouse_target_last_changed = current_time;

	if (webkit_hit_test_result_context_is_link(result)) {
		uri = webkit_hit_test_result_get_link_uri(result);
	} else if (webkit_hit_test_result_context_is_image(result)) {
		uri = webkit_hit_test_result_get_image_uri(result);
	}

	if (g_strcmp0(uri, "") == 0) {
		// We only send one empty URL (i.e. current URL) in a row to avoid
		// flickering when hovering over an input field or the scroll bar.
		if (mouse_target_last_was_empty == false) {
			mouse_target_last_was_empty = true;
		} else {
			mouse_target_last_was_empty = true;
			return;
		}
	} else {
		mouse_target_last_was_empty = false;
	}

	GVariant *arg = g_variant_new("(s)", uri);
	g_message("RPC message: %s %s", method_name, g_variant_print(arg, TRUE));

	g_dbus_connection_call(state.connection,
		CORE_NAME, CORE_OBJECT_PATH, CORE_INTERFACE,
		method_name,
		arg,
		NULL, G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL, NULL);
	// 'uri' is freed automatically.
}

static gboolean buffer_load_failed_with_tls_errors(WebKitWebView *web_view,
	gchar *failing_uri, GTlsCertificate *certificate, GTlsCertificateFlags errors,
	gpointer user_data) {
	Buffer *buffer = (Buffer *)user_data;
	SoupURI *soup_uri = soup_uri_new(failing_uri);
	const gchar *const *whitelist_hosts = buffer->_certificate_whitelist_hosts;
	gboolean match = FALSE;

	while (whitelist_hosts != NULL && whitelist_hosts[0] != NULL) {
		if (g_strcmp0(soup_uri->host, whitelist_hosts[0]) == 0) {
			webkit_web_context_allow_tls_certificate_for_host(
				webkit_web_view_get_context(buffer->web_view), certificate, soup_uri->host);
			webkit_web_view_load_uri(buffer->web_view, failing_uri);
			match = TRUE;
			break;
		}
		whitelist_hosts++;
	}

	soup_uri_free(soup_uri);

	return match;
}

Buffer *buffer_init(const char *cookie_file) {
	Buffer *buffer = calloc(1, sizeof (Buffer));
	if (buffer == NULL) {
		g_error("Failed to allocate buffer");
		exit(1);
	}
	WebKitWebContext *context = webkit_web_context_new();
	webkit_web_context_set_process_model(context, WEBKIT_PROCESS_MODEL_MULTIPLE_SECONDARY_PROCESSES);
	webkit_web_context_set_cache_model(context, WEBKIT_CACHE_MODEL_WEB_BROWSER);

	buffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new_with_context(context));

	// WebKitGTK+ 2.24 seems to have a bug when transitionning hardware
	// acceleration policy (for composition), so we need to force it for buffers.
	// It seems that there is no need to force it for the minibuffer.
	// TODO: This could be a problem for systems where hardware acceleration is
	// not available.
	WebKitSettings *settings = webkit_web_view_get_settings(buffer->web_view);
	webkit_settings_set_hardware_acceleration_policy(settings, WEBKIT_HARDWARE_ACCELERATION_POLICY_ALWAYS);

	buffer_set_cookie_file(buffer, cookie_file);

	g_signal_connect(buffer->web_view, "load-changed",
		G_CALLBACK(buffer_web_view_load_changed), buffer);
	g_signal_connect(buffer->web_view, "decide-policy",
		G_CALLBACK(buffer_web_view_decide_policy), buffer);
	g_signal_connect(buffer->web_view, "web-process-crashed",
		G_CALLBACK(buffer_web_view_web_process_crashed), buffer);
	g_signal_connect(buffer->web_view, "mouse-target-changed",
		G_CALLBACK(buffer_mouse_target_changed), buffer);
	g_signal_connect(buffer->web_view, "notify::uri",
		G_CALLBACK(buffer_notify_uri), buffer);

	g_signal_connect(context, "download-started", G_CALLBACK(buffer_web_view_download_started), buffer);

	// Mouse events are captured by the web view first, so we must intercept them here.
	g_signal_connect(buffer->web_view, "button-press-event", G_CALLBACK(window_button_event), buffer);
	g_signal_connect(buffer->web_view, "button-release-event", G_CALLBACK(window_button_event), buffer);
	g_signal_connect(buffer->web_view, "scroll-event", G_CALLBACK(window_scroll_event), buffer);

        // TLS certificate handling
	g_signal_connect(buffer->web_view, "load-failed-with-tls-errors",
		G_CALLBACK(buffer_load_failed_with_tls_errors), buffer);

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
	buffer->identifier = NULL; // In case buffer is referenced elsewhere.
	g_free(buffer);
}

void buffer_load(Buffer *buffer, const char *uri) {
	webkit_web_view_load_uri(buffer->web_view, uri);
}

static void buffer_javascript_callback(GObject *object, GAsyncResult *result,
	gpointer user_data) {
	BufferInfo *buffer_info = (BufferInfo *)user_data;
	javascript_transform_result(object, result, buffer_info->buffer->identifier,
		buffer_info->callback_id,
		"buffer_javascript_call_back");
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
	return g_strdup_printf("%i", buffer_info->callback_id);
}

void buffer_set_proxy(Buffer *buffer, WebKitNetworkProxyMode mode,
	const gchar *proxy_uri, const gchar *const *ignore_hosts) {
	WebKitWebContext *context = webkit_web_view_get_context(buffer->web_view);

	WebKitNetworkProxySettings *settings = NULL;
	if (mode == WEBKIT_NETWORK_PROXY_MODE_CUSTOM) {
		settings = webkit_network_proxy_settings_new(proxy_uri, ignore_hosts);
	}
	webkit_web_context_set_network_proxy_settings(context, mode, settings);
	buffer->_proxy_mode = mode;
	buffer->_proxy_uri = strdup(proxy_uri);
	buffer->_ignore_hosts = ignore_hosts;
	g_debug("Proxy set");
}

void buffer_get_proxy(Buffer *buffer, WebKitNetworkProxyMode *mode,
	const gchar **proxy_uri, const gchar *const **ignore_hosts) {
	*mode = buffer->_proxy_mode;
	*proxy_uri = "";
	*ignore_hosts = NULL;
	if (*mode == WEBKIT_NETWORK_PROXY_MODE_CUSTOM) {
		*proxy_uri = buffer->_proxy_uri;
		*ignore_hosts = buffer->_ignore_hosts;
	}
}

void buffer_set_certificate_whitelist(Buffer *buffer,
	const gchar *const *whitelist_hosts) {
	buffer->_certificate_whitelist_hosts = whitelist_hosts;
	webkit_web_view_reload(buffer->web_view);
	g_debug("Certificate whitelist set");
}

void buffer_get_certificate_whitelist(Buffer *buffer, const gchar *const **whitelist_hosts) {
	*whitelist_hosts = buffer->_certificate_whitelist_hosts;
}

void buffer_set(Buffer *buffer, const gchar *setting, gboolean value) {
	WebKitSettings *settings = webkit_web_view_get_settings(buffer->web_view);
	g_object_set(G_OBJECT(settings), setting, value, NULL);
}
