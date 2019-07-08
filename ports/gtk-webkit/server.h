/*
Copyright © 2018-2019 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <stdarg.h>
#include <glib.h>
#include <libsoup/soup.h>

#include "window.h"

typedef GVariant * (*ServerCallback) (GVariant *);

static GVariant *server_window_make(GVariant *parameters) {
	// TODO: Rename a_key to key.
	const char *a_key = NULL;
	g_variant_get(parameters, "(&s)", &a_key);
	g_message("Method parameter(s): %s", a_key);

	Window *window = window_init();
	g_hash_table_insert(state.windows, strdup(a_key), window);
	window->identifier = strdup(a_key);
	window->minibuffer->parent_window_identifier = strdup(window->identifier);
	g_message("Method result(s): window id %s", window->identifier);
	return g_variant_new("(s)", window->identifier);
}

static GVariant *server_window_set_title(GVariant *parameters) {
	const char *a_key = NULL;
	const char *title = NULL;
	g_variant_get(parameters, "(&s&s)", &a_key, &title);
	g_message("Method parameter(s): %s, %s", a_key, title);

	Window *window = g_hash_table_lookup(state.windows, a_key);
	if (!window) {
		return g_variant_new("(b)", FALSE);
	}
	window_set_title(window, title);
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_window_delete(GVariant *parameters) {
	const char *a_key = NULL;
	g_variant_get(parameters, "(&s)", &a_key);
	g_message("Method parameter(s): %s", a_key);

	g_hash_table_remove(state.windows, a_key);
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_window_active(GVariant *_parameters) {
	// TODO: If we run a GTK application, then we could call
	// gtk_application_get_active_window() and get the identifier from there.
	// We could also lookup the active window in gtk_window_list_toplevels().
	GHashTableIter iter;
	gpointer key, value;

	char *id = "<no active window>";
	g_hash_table_iter_init(&iter, state.windows);
	while (g_hash_table_iter_next(&iter, &key, &value)) {
		Window *window = (Window *)value;
		if (gtk_window_is_active(GTK_WINDOW(window->base))) {
			id = window->identifier;
			break;
		}
	}

	g_message("Method parameter(s): %s", id);
	return g_variant_new("(s)", id);
}

static GVariant *server_window_exists(GVariant *parameters) {
	const char *a_key = NULL;
	g_variant_get(parameters, "(&s)", &a_key);
	g_message("Method parameter(s): %s", a_key);

	Window *window = g_hash_table_lookup(state.windows, a_key);
	if (!window) {
		return g_variant_new("(b)", FALSE);
	}
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_window_set_active_buffer(GVariant *parameters) {
	const char *window_id = NULL;
	const char *buffer_id = NULL;
	g_variant_get(parameters, "(&s&s)", &window_id, &buffer_id);
	g_message("Method parameter(s): window id %s, buffer id %s", window_id, buffer_id);

	Window *window = g_hash_table_lookup(state.windows, window_id);
	Buffer *buffer = g_hash_table_lookup(state.buffers, buffer_id);
	if (window == NULL) {
		g_warning("Non-existent window %s", window_id);
		return g_variant_new("(b)", FALSE);
	}
	if (buffer == NULL) {
		g_warning("Non-existent buffer %s", buffer_id);
		return g_variant_new("(b)", FALSE);
	}
	window_set_active_buffer(window, buffer);
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_buffer_make(GVariant *parameters) {
	const char *a_key = NULL;
	GVariant *options;
	g_variant_get(parameters, "(&s@a{ss})", &a_key, &options);
	g_message("Method parameter(s): buffer ID %s, options %s", a_key,
		g_variant_print(options, TRUE));

	char *cookies_path = NULL;
	g_variant_lookup(options, "cookies-path", "s", &cookies_path);

	Buffer *buffer = buffer_init(cookies_path);

	g_hash_table_insert(state.buffers, strdup(a_key), buffer);
	buffer->identifier = strdup(a_key);
	g_message("Method result(s): buffer id %s", buffer->identifier);
	return g_variant_new("(s)", buffer->identifier);
}

static GVariant *server_buffer_delete(GVariant *parameters) {
	const char *a_key = NULL;
	g_variant_get(parameters, "(&s)", &a_key);
	g_message("Method parameter(s): %s", a_key);
	Buffer *buffer = g_hash_table_lookup(state.buffers, a_key);

	// In case the 'buffer' field of some window points to it, set it to NULL to
	// prevent illegal memory access.
	GHashTableIter iter;
	gpointer key, value;
	g_hash_table_iter_init(&iter, state.windows);
	while (g_hash_table_iter_next(&iter, &key, &value)) {
		Window *window = (Window *)value;
		if (window->buffer == buffer) {
			window->buffer = NULL;
			break;
		}
	}

	g_hash_table_remove(state.buffers, a_key);
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_buffer_load(GVariant *parameters) {
	const char *buffer_id = NULL;
	const char *uri = NULL;
	g_variant_get(parameters, "(&s&s)", &buffer_id, &uri);
	g_message("Method parameter(s): buffer id %s, URI %s", buffer_id, uri);

	Buffer *buffer = g_hash_table_lookup(state.buffers, buffer_id);
	if (!buffer) {
		g_warning("Non-existent buffer %s", buffer_id);
		return g_variant_new("(b)", FALSE);
	}

	buffer_load(buffer, uri);
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_buffer_evaluate(GVariant *parameters) {
	const char *buffer_id = NULL;
	const char *javascript = NULL;
	g_variant_get(parameters, "(&s&s)", &buffer_id, &javascript);
	g_message("Method parameter(s): buffer id %s, javascript (...)", buffer_id);
	g_debug("Javascript: \"%s\"", javascript);

	Buffer *buffer = g_hash_table_lookup(state.buffers, buffer_id);
	if (!buffer) {
		g_warning("Non-existent buffer %s", buffer_id);
		return g_variant_new("(s)", "");
	}
	char *callback_id = buffer_evaluate(buffer, javascript);
	g_message("Method result(s): callback id %s", callback_id);
	GVariant *callback_variant = g_variant_new("(s)", callback_id);
	g_free(callback_id);
	return callback_variant;
}

static GVariant *server_window_set_minibuffer_height(GVariant *parameters) {
	const char *window_id = NULL;
	int minibuffer_height = 0;
	g_variant_get(parameters, "(&si)", &window_id,
		&minibuffer_height);
	g_message("Method parameter(s): window id %s, minibuffer height %i", window_id,
		minibuffer_height);

	Window *window = g_hash_table_lookup(state.windows, window_id);
	if (!window) {
		g_warning("Non-existent window %s", window_id);
		return g_variant_new("(x)", 0);
	}
	gint64 preferred_height = window_set_minibuffer_height(window, minibuffer_height);
	g_message("Method result(s): minibuffer preferred height %li", preferred_height);
	return g_variant_new("(x)", preferred_height);
}

static GVariant *server_minibuffer_evaluate(GVariant *parameters) {
	const char *window_id = NULL;
	const char *javascript = NULL;
	g_variant_get(parameters, "(&s&s)", &window_id, &javascript);
	g_message("Method parameter(s): window id %s, javascript (...)", window_id);
	g_debug("Javascript: \"%s\"", javascript);

	Window *window = g_hash_table_lookup(state.windows, window_id);
	Minibuffer *minibuffer = window->minibuffer;
	char *callback_id = minibuffer_evaluate(minibuffer, javascript);
	g_message("Method result(s): callback id %s", callback_id);
	GVariant *callback_variant = g_variant_new("(s)", callback_id);
	g_free(callback_id);
	return callback_variant;
}

static GVariant *server_generate_input_event(GVariant *parameters) {
	const char *window_id = NULL;
	guint hardware_keycode = 0; // We are given an integer "i", not a uint16.
	guint modifiers = 0; // modifiers
	guint keyval = 0;
	gdouble x = -1;
	gdouble y = -1;
	gboolean released = false;

	{
		if (!g_variant_check_format_string(parameters, "(siasidd)", FALSE)) {
			g_warning("Malformed input event: %s", g_variant_get_type_string(parameters));
			return g_variant_new("(b)", FALSE);
		}
		GVariantIter *iter;
		g_variant_get(parameters, "(siasidd)", &window_id, &hardware_keycode,
			&iter, &keyval, &x, &y);

		gchar *str = NULL;
		while (g_variant_iter_loop(iter, "s", &str)) {
			if (g_strcmp0(str, "R") == 0) {
				released = true;
			} else {
				guint mod = window_string_to_modifier(str);
				if (mod != 0) {
					modifiers |= mod;
				}
			}
		}
		g_variant_iter_free(iter);
	}

	g_message("Method parameter(s): window id '%s', hardware_keycode %i, keyval %i, modifiers %i",
		window_id, hardware_keycode, keyval, modifiers);

	Window *window = g_hash_table_lookup(state.windows, window_id);
	if (!window) {
		g_warning("Non-existent window %s", window_id);
		return g_variant_new("(b)", FALSE);
	}

	GdkEvent event;
	if (x != -1) {
		// TODO: Rename hardware_keycode to something that fits mouse buttons.
		if (hardware_keycode != 0) {
			GdkEventScroll event_scroll = {
				.state = modifiers,
				.direction = hardware_keycode,
				.x = x,
				.y = y,
				.delta_x = 0,
				.delta_y = 0,
			};
			if (keyval == 4) {
				event_scroll.delta_x = 0;
				event_scroll.delta_y = -1;
			} else if (keyval == 5) {
				event_scroll.delta_x = 0;
				event_scroll.delta_y = 1;
			} else if (keyval == 6) {
				event_scroll.delta_x = -1;
				event_scroll.delta_y = 0;
			} else if (keyval == 7) {
				event_scroll.delta_x = 1;
				event_scroll.delta_y = 0;
			}
			event = (GdkEvent)event_scroll;
			event.type = GDK_SCROLL;
		} else {
			GdkEventButton event_button = {
				.button = keyval,
				.state = modifiers,
				.x = x,
				.y = y,
			};
			event = (GdkEvent)event_button;
			if (released) {
				event.type = GDK_BUTTON_RELEASE;
			} else {
				event.type = GDK_BUTTON_PRESS;
			}
		}
	} else {
		GdkEventKey event_key = {
			.hardware_keycode = hardware_keycode,
			.keyval = keyval,
			.state = modifiers,
			.group = 0,
			.string = NULL,
			.is_modifier = 0,
		};
		event = (GdkEvent)event_key;
		if (released) {
			event.type = GDK_KEY_RELEASE;
		} else {
			event.type = GDK_KEY_PRESS;
		}
	}

	WindowEvent *window_event = g_new(WindowEvent, 1);
	window_event->event = event;
	window_event->window = window;
	window_generate_input_event(window_event);

	return g_variant_new("(b)", TRUE);
}

// TODO: Implement introspection for D-Bus.
/*
static GVariant *server_list_methods(SoupXMLRPCParams *_params) {
        GHashTableIter iter;
        gpointer key;

        GVariantBuilder builder;
        g_variant_builder_init(&builder, G_VARIANT_TYPE_ARRAY);

        g_hash_table_iter_init(&iter, state.server_callbacks);
        while (g_hash_table_iter_next(&iter, &key, NULL)) {
                g_variant_builder_add_parsed(&builder, "%s", key);
        }

        return g_variant_builder_end(&builder);
}
*/

/* ITER cannot be used after this call.  RESULT must be freed. */
GList *server_unwrap_string_list(GVariantIter *iter) {
	gchar *str = NULL;
	GList *result = NULL;
	while (g_variant_iter_loop(iter, "s", &str)) {
		result = g_list_prepend(result, strdup(str));
	}
	result = g_list_reverse(result);
	g_variant_iter_free(iter);
	return result;
}

/* The GList can be freed but not its elements which are shared with the result.
The result must be freed. */
char **server_string_list_to_array_pointer(GList *list) {
	guint length = g_list_length(list);
	if (length == 0) {
		return NULL;
	}

	char **result = g_new(char *, 1+length);
	GList *old_list = list;
	int i = 0;
	while (list != NULL) {
		result[i] = list->data;
		i++;
		list = list->next;
	}
	// Make sure it's NULL-terminated so that we can detect the end of the array.
	result[g_list_length(old_list)] = NULL;
	return result;
}

static GVariant *server_set_proxy(GVariant *parameters) {
	GList *buffer_ids = NULL;
	const char *mode = NULL;
	const char *proxy_uri = NULL;
	char **ignore_hosts = NULL;
	// ignore_hosts is passed as a list of string variants.
	{
		GVariantIter *iter;
		GVariantIter *iter_buffers;
		g_variant_get(parameters, "(as&s&sas)", &iter_buffers, &mode, &proxy_uri, &iter);

		buffer_ids = server_unwrap_string_list(iter_buffers);
		GList *ignore_hosts_list = server_unwrap_string_list(iter);
		ignore_hosts = server_string_list_to_array_pointer(ignore_hosts_list);
		g_list_free(ignore_hosts_list);
	}

	{
		gchar *pretty_ignore_hosts = g_strjoinv(",", ignore_hosts);
		char **buffer_ids_buf = server_string_list_to_array_pointer(buffer_ids);
		gchar *pretty_buffer_ids = g_strjoinv(",", buffer_ids_buf);
		g_message("Method parameter(s): buffer ID(s) %s, set proxy=%s, URI=%s, ignore_hosts=%s",
			pretty_buffer_ids, mode, proxy_uri, pretty_ignore_hosts);
		g_free(buffer_ids_buf);
		g_free(pretty_buffer_ids);
		g_free(pretty_ignore_hosts);
	}

	int mode_enum = WEBKIT_NETWORK_PROXY_MODE_DEFAULT;
	if (g_strcmp0(mode, "custom") == 0) {
		mode_enum = WEBKIT_NETWORK_PROXY_MODE_CUSTOM;
	} else if (g_strcmp0(mode, "none") == 0) {
		mode_enum = WEBKIT_NETWORK_PROXY_MODE_NO_PROXY;
	}

	while (buffer_ids != NULL) {
		Buffer *buffer = g_hash_table_lookup(state.buffers, buffer_ids->data);
		buffer_set_proxy(buffer, mode_enum, proxy_uri, (const char *const *)ignore_hosts);
		buffer_ids = buffer_ids->next;
	}

	// TODO: Don't free ignore_hosts if we need to store them in the Buffer struct?
	// More elegant way to do this?
	/* g_free(ignore_hosts); */
	g_list_free(buffer_ids);
	return g_variant_new("(b)", TRUE);
}

static GVariant *server_get_proxy(GVariant *parameters) {
	const char *a_key = NULL;
	g_variant_get(parameters, "(&s)", &a_key);
	g_message("Method parameter(s): %s", a_key);

	WebKitNetworkProxyMode mode;
	const gchar *proxy_uri = NULL;
	const gchar *const *ignore_hosts = NULL;
	buffer_get_proxy(g_hash_table_lookup(state.buffers, a_key),
		&mode, &proxy_uri, &ignore_hosts);

	char *mode_string = "default"; // System proxy.
	if (mode == WEBKIT_NETWORK_PROXY_MODE_CUSTOM) {
		mode_string = "custom";
	} else if (mode == WEBKIT_NETWORK_PROXY_MODE_NO_PROXY) {
		mode_string = "none";
	}

	// TODO: Shouldn't the hosts be a sublist?
	GVariantBuilder builder;
	g_variant_builder_init(&builder, G_VARIANT_TYPE_TUPLE);
	g_variant_builder_add(&builder, "s", mode_string);
	g_variant_builder_add(&builder, "s", proxy_uri);
	while (ignore_hosts != NULL && ignore_hosts[0] != NULL) {
		g_variant_builder_add(&builder, "s", ignore_hosts[0]);
		ignore_hosts++;
	}

	return g_variant_builder_end(&builder);
}

// TODO: Some settings take an integer or a string (e.g. the user agent).
static GVariant *server_buffer_set(GVariant *parameters) {
	const char *id = NULL;
	const char *setting = NULL;
	gboolean value;
	g_variant_get(parameters, "(&s&sb)", &id, &setting, &value);
	g_message("Method parameter(s): buffer ID %s, setting %s, value %i", id, setting, value);
	buffer_set(g_hash_table_lookup(state.buffers, id), setting, value);

	return g_variant_new("(b)", TRUE);
}

void server_handler(GDBusConnection *_connection,
	const gchar *_sender,
	const gchar *_object_path,
	const gchar *_interface_name,
	const gchar *method_name,
	GVariant *parameters,
	GDBusMethodInvocation *invocation,
	gpointer _user_data) {
	ServerCallback callback = NULL;
	gboolean found = g_hash_table_lookup_extended(state.server_callbacks, method_name,
			NULL, (gpointer *)&callback);
	if (!found) {
		g_warning("Unknown method: %s", method_name);
		return;
	}
	g_message("Method name: %s", method_name);

	GVariant *result = callback(parameters);

	// 'result' is floating and soup_xmlrpc_message_set_response will consume it.
	g_dbus_method_invocation_return_value(invocation, result);
}

static const GDBusInterfaceVTable interface_vtable = {
	server_handler,
	NULL,
	NULL
};

void start_server(GDBusConnection *connection,
	const gchar *name,
	gpointer user_data) {
	GDBusNodeInfo *introspection_data =
		g_dbus_node_info_new_for_xml(state.introspection_xml, NULL);

	guint registration_id = g_dbus_connection_register_object(connection,
			PLATFORM_PORT_OBJECT_PATH,
			introspection_data->interfaces[0],
			&interface_vtable,
			NULL,                                 /* user_data */
			NULL,                                 /* user_data_free_func */
			NULL);                                /* GError** */

	state.registration_id = registration_id;

	// Initialize global state.
	state.server_callbacks = g_hash_table_new(g_str_hash, g_str_equal);
	state.windows = g_hash_table_new_full(g_str_hash, g_str_equal,
			&g_free, (GDestroyNotify)&window_delete);
	state.buffers = g_hash_table_new_full(g_str_hash, g_str_equal,
			&g_free, (GDestroyNotify)&buffer_delete);

	GError *error = NULL;
	state.connection = g_bus_get_sync(G_BUS_TYPE_SESSION, NULL, &error);
	if (error != NULL) {
		g_error("Cannot connect to D-Bus: %s", error->message);
		exit(1);
	}

	// Register callbacks.
	g_hash_table_insert(state.server_callbacks, "window_make", &server_window_make);
	g_hash_table_insert(state.server_callbacks, "window_set_title", &server_window_set_title);
	g_hash_table_insert(state.server_callbacks, "window_delete", &server_window_delete);
	g_hash_table_insert(state.server_callbacks, "window_active", &server_window_active);
	g_hash_table_insert(state.server_callbacks, "window_exists", &server_window_exists);
	g_hash_table_insert(state.server_callbacks, "window_set_active_buffer", &server_window_set_active_buffer);
	g_hash_table_insert(state.server_callbacks, "window_set_minibuffer_height", &server_window_set_minibuffer_height);
	g_hash_table_insert(state.server_callbacks, "buffer_make", &server_buffer_make);
	g_hash_table_insert(state.server_callbacks, "buffer_delete", &server_buffer_delete);
	g_hash_table_insert(state.server_callbacks, "buffer_load", &server_buffer_load);
	g_hash_table_insert(state.server_callbacks, "buffer_evaluate_javascript", &server_buffer_evaluate);
	g_hash_table_insert(state.server_callbacks, "minibuffer_evaluate_javascript", &server_minibuffer_evaluate);
	g_hash_table_insert(state.server_callbacks, "generate_input_event", &server_generate_input_event);
	g_hash_table_insert(state.server_callbacks, "set_proxy", &server_set_proxy); // TODO: Rename buffer_set_proxy?
	g_hash_table_insert(state.server_callbacks, "get_proxy", &server_get_proxy);
	g_hash_table_insert(state.server_callbacks, "buffer_set", &server_buffer_set);
}

void stop_server() {
	g_hash_table_unref(state.windows);
	g_hash_table_unref(state.buffers);
	g_hash_table_unref(state.server_callbacks);
	g_dbus_connection_unregister_object(state.connection, state.registration_id);
}
