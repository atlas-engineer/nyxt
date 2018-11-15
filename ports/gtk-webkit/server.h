/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <glib.h>
#include <libsoup/soup.h>

#include "autokey-dictionary.h"
#include "window.h"

typedef GVariant * (*ServerCallback) (SoupXMLRPCParams *);

// TODO: Make local?  Use struct?
static AutokeyDictionary *windows;
static AutokeyDictionary *buffers;
static GHashTable *server_callbacks;

// TODO: Prefix all those functions with "server_"?  Use separate .h?
static GVariant *server_window_make(SoupXMLRPCParams *_params) {
	Window *window = window_init();
	window->identifier = akd_insert_element(windows, window);
	return g_variant_new_string(window->identifier);
}

static GVariant *server_window_delete(SoupXMLRPCParams *params) {
	GError *error = NULL;
	GVariant *variant = soup_xmlrpc_params_parse(params, NULL, &error);
	if (error) {
		g_warning("Malformed method parameters: %s", error->message);
		return g_variant_new_boolean(FALSE);
	}
	if (!g_variant_check_format_string(variant, "av", FALSE)) {
		g_warning("Malformed parameter value: %s", g_variant_get_type_string(variant));
		return g_variant_new_boolean(FALSE);
	}
	// Variant type string is "av", and the embedded "v"'s type string is "s".
	const char *a_key = g_variant_get_string(
		g_variant_get_variant(
			g_variant_get_child_value(variant, 0)),
		NULL);
	g_debug("Method parameter: %s", a_key);

	Window *window = akd_object_for_key(windows, a_key);
	window_delete(window);
	akd_remove_object_for_key(windows, a_key);
	return g_variant_new_boolean(TRUE);
}

static GVariant *server_window_active(SoupXMLRPCParams *_params) {
	// TODO: If we run a GTK application, then we could call
	// gtk_application_get_active_window() and get the identifier from there.
	// We could also lookup the active window in gtk_window_list_toplevels().
	GHashTableIter iter;
	gpointer key, value;

	g_hash_table_iter_init(&iter, windows->_dict);
	while (g_hash_table_iter_next(&iter, &key, &value)) {
		Window *window = (Window *)value;
		if (gtk_window_is_active(GTK_WINDOW(window->base))) {
			g_debug("Active window identifier: %s", window->identifier);
			return g_variant_new_string(window->identifier);
		}
	}

	// TODO: Is "-1" a good name for a window that does not exist?
	g_debug("No active window");
	return g_variant_new_string("-1");
}

static GVariant *server_set_active_buffer(SoupXMLRPCParams *params) {
	GError *error = NULL;
	GVariant *variant = soup_xmlrpc_params_parse(params, NULL, &error);
	if (error) {
		g_warning("Malformed method parameters: %s", error->message);
		return g_variant_new_boolean(FALSE);
	}
	if (!g_variant_check_format_string(variant, "av", FALSE)) {
		g_warning("Malformed parameter value: %s", g_variant_get_type_string(variant));
		return g_variant_new_boolean(FALSE);
	}
	// Variant type string is "av", and the two embedded "v"'s type string are "s".
	const char *window_id = g_variant_get_string(
		g_variant_get_variant(
			g_variant_get_child_value(variant, 0)),
		NULL);
	const char *buffer_id = g_variant_get_string(
		g_variant_get_variant(
			g_variant_get_child_value(variant, 0)),
		NULL);
	g_debug("Method parameter: window_id %s, buffer_id %s", window_id, buffer_id);

	Window *window = akd_object_for_key(windows, window_id);
	Buffer *buffer = akd_object_for_key(buffers, buffer_id);
	window_set_active_buffer(window, buffer);
	return g_variant_new_boolean(TRUE);
}

static GVariant *server_buffer_make(SoupXMLRPCParams *_params) {
	Buffer *buffer = buffer_init();
	buffer->identifier = akd_insert_element(buffers, buffer);
	return g_variant_new_string(buffer->identifier);
}

static GVariant *server_buffer_delete(SoupXMLRPCParams *params) {
	GError *error = NULL;
	GVariant *variant = soup_xmlrpc_params_parse(params, NULL, &error);
	if (error) {
		g_warning("Malformed method parameters: %s", error->message);
		return g_variant_new_boolean(FALSE);
	}
	if (!g_variant_check_format_string(variant, "av", FALSE)) {
		g_warning("Malformed parameter value: %s", g_variant_get_type_string(variant));
	}
	// Variant type string is "av", and the embedded "v"'s type string is "s".
	const char *a_key = g_variant_get_string(
		g_variant_get_variant(
			g_variant_get_child_value(variant, 0)),
		NULL);
	g_debug("Method parameter: %s", a_key);

	Buffer *buffer = akd_object_for_key(buffers, a_key);
	buffer_delete(buffer);
	akd_remove_object_for_key(buffers, a_key);
	return g_variant_new_boolean(TRUE);
}

static GVariant *server_buffer_evaluate(SoupXMLRPCParams *params) {
	GError *error = NULL;
	GVariant *variant = soup_xmlrpc_params_parse(params, NULL, &error);
	if (error) {
		g_warning("Malformed method parameters: %s", error->message);
		return g_variant_new_boolean(FALSE);
	}
	if (!g_variant_check_format_string(variant, "av", FALSE)) {
		g_warning("Malformed parameter value: %s", g_variant_get_type_string(variant));
		return g_variant_new_boolean(FALSE);
	}
	// Variant type string is "av", and the two embedded "v"'s type string are "s".
	const char *buffer_id = g_variant_get_string(
		g_variant_get_variant(
			g_variant_get_child_value(variant, 0)),
		NULL);
	const char *javascript = g_variant_get_string(
		g_variant_get_variant(
			g_variant_get_child_value(variant, 0)),
		NULL);
	g_debug("Method parameter: buffer_id %s, javascript %s", buffer_id, javascript);

	Buffer *buffer = akd_object_for_key(buffers, buffer_id);
	char *operation_result = buffer_evaluate(buffer, javascript);
	return g_variant_new_string(operation_result);
}

static void server_handler(SoupServer *server, SoupMessage *msg,
	const char *path, GHashTable *query,
	SoupClientContext *context, gpointer data) {
	// Log request.
	{
		const char *name, *value;
		SoupMessageHeadersIter iter;
		GString *pretty_message = g_string_new("HTTP request:\n");
		g_string_append_printf(pretty_message, "%s %s HTTP/1.%d\n", msg->method, path,
			soup_message_get_http_version(msg));
		soup_message_headers_iter_init(&iter, msg->request_headers);
		while (soup_message_headers_iter_next(&iter, &name, &value)) {
			g_string_append_printf(pretty_message, "%s: %s\n", name, value);
		}
		if (msg->request_body->length == 0) {
			g_warning("Empty HTTP request");
			return;
		}
		g_string_append_printf(pretty_message, "%s", msg->request_body->data);
		g_debug("%s", pretty_message->str);
		g_string_free(pretty_message, TRUE);
	}

	SoupXMLRPCParams *params = NULL;
	GError *error = NULL;
	char *method_name = soup_xmlrpc_parse_request(msg->request_body->data,
			msg->request_body->length,
			&params,
			&error);
	if (error) {
		g_warning("Malformed XMLRPC request: %s", error->message);
		return;
	}

	ServerCallback callback = NULL;
	gboolean found = g_hash_table_lookup_extended(server_callbacks, method_name,
			NULL, (gpointer *)&callback);
	if (!found) {
		g_warning("Unknown method: %s", method_name);
		return;
	}
	g_debug("Method name: %s", method_name);

	GVariant *operation_result = callback(params);

	soup_xmlrpc_params_free(params);

	soup_xmlrpc_message_set_response(msg, operation_result, &error);
	if (error) {
		g_warning("Failed to set XMLRPC response: %s", error->message);
	}

	g_debug("Response: %d %s", msg->status_code, msg->reason_phrase);
}

void start_server() {
	// TODO: Server logging?
	// TODO: libsoup's examples don't unref the server.  Should we?
	SoupServer *server = soup_server_new(
		/* SOUP_SERVER_SERVER_HEADER, APPNAME, */
		NULL);

	GError *error = NULL;
	soup_server_listen_all(server, 8082, 0, &error);
	if (error) {
		g_printerr("Unable to create server: %s\n", error->message);
		exit(1);
	}
	g_debug("Starting XMLRPC server");
	soup_server_add_handler(server, NULL, server_handler, NULL, NULL);

	// Register callbacks.
	server_callbacks = g_hash_table_new(g_str_hash, g_str_equal);
	g_hash_table_insert(server_callbacks, "window.make", &server_window_make);
	g_hash_table_insert(server_callbacks, "window.delete", &server_window_delete);
	g_hash_table_insert(server_callbacks, "window.active", &server_window_active);
	g_hash_table_insert(server_callbacks, "window.set.active.buffer", &server_set_active_buffer);
	g_hash_table_insert(server_callbacks, "buffer.make", &server_buffer_make);
	g_hash_table_insert(server_callbacks, "buffer.delete", &server_buffer_delete);
	// TODO: Change API to "buffer.evaluate".
	g_hash_table_insert(server_callbacks, "buffer.execute.javascript", &server_buffer_evaluate);

	// Global indentifiers.
	windows = akd_init(NULL);
	buffers = akd_init(NULL);
}

void stop_server() {
	akd_free(windows);
}
