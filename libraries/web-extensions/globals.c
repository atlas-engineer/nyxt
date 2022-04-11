// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"

/** PAGE
 *
 * The WebKitWebPage currently open.
 */
WebKitWebPage *PAGE;

/** EXTENSION
 *
 * The WebKitWebExtension this code runs in.
 */
WebKitWebExtension *EXTENSION;

/** EXTENSIONS_DATA
 *
 * The data to manage all the extensions loaded into the view.
 *
 * Is a GHashTable with string keys (extension names) and
 * ExtensionData objects values.
 */
GHashTable *EXTENSIONS_DATA;

int IS_PRIVILEGED;

/** DATA_COUNTER
 *
 * The last ID in DATA extension-to-browser message table (see
 * get_next_data_counter and SEND_MESSAGE_RETURN_ID.)
 */
unsigned long int DATA_COUNTER;

/** DATA
 *
 * A hash table of all the messages from extensions to browser and
 * vice versa. Keys are ulong integer IDs (see DATA_COUNTER), values
 * are strings with the data JSON (of course when there's a reply.)
 */
GHashTable *DATA;

unsigned long int
get_next_data_counter ()
{
        return ++DATA_COUNTER;
}

/** extensions_data_add_from_json
 *
 * Fill the EXTENSIONS_DATA with the extensions' metadata from json.
 */
void
extensions_data_add_from_json(const char *json)
{
        JSCContext *dummy_context = jsc_context_new();
        JSCValue *object = jsc_value_new_from_json(dummy_context, json);
        char **properties = jsc_value_object_enumerate_properties(object);
        char **property;
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        if (properties) {
                for (property = properties; *property != NULL; property++){
                        ExtensionData *extension;
                        const char *name = *property;
                        JSCValue *data = jsc_value_object_get_property(object, *property);
                        char *manifest = jsc_value_to_string(
                                jsc_value_object_get_property_at_index(data, 1));
                        JSCValue *files = jsc_value_object_get_property_at_index(data, 3);
                        char *id = jsc_value_to_string(
                                jsc_value_object_get_property_at_index(data, 0));
                        int is_privileged = jsc_value_to_boolean(jsc_value_object_get_property_at_index(data, 2));
                        char *tab_id = jsc_value_to_string(jsc_value_object_get_property_at_index(data, 4));
                        WebKitScriptWorld *world;
                        if (is_privileged && !IS_PRIVILEGED) {
                                IS_PRIVILEGED = is_privileged;
                                world = webkit_script_world_get_default();
                        } else {
                                world = webkit_script_world_new_with_name(name);
                        }
                        extension = g_malloc(sizeof(ExtensionData));
                        extension->name = (char*) name;
                        extension->manifest = manifest;
                        extension->files = files;
                        extension->extension_id = id;
                        extension->is_injected = 0;
                        extension->is_privileged = is_privileged;
                        extension->world = world;
                        extension->tab_id = tab_id;
                        g_hash_table_insert(EXTENSIONS_DATA, (void*) name, extension);
                }
        }
}

/** get_extension_world
 *
 * A helper to get the world associated with the extensions_name-named
 * extension.
 *
 * Returns default world if the extension_name is NULL.
 */
WebKitScriptWorld *
get_extension_world (char* extension_name)
{
        if (extension_name) {
                ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
                return data->world;
        } else {
                return webkit_script_world_get_default();
        }
}

/** get_extension_context
 *
 * A helper to get the JSCContext associated with the
 * extensions_name-named extension.
 *
 * Returns default world-associated JCContext if the extension_name is NULL.
 */
JSCContext *
get_extension_context (char* extension_name)
{
        WebKitScriptWorld *world = get_extension_world(extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        return webkit_frame_get_js_context_for_script_world(frame, world);
}

/** get_extension_id
 *
 * A helper to get the unique ID of the extension by its
 * extension_name.
 */
char *
get_extension_id (char* extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        return data->extension_id;
}

/*** Host permission checking.
 *
 * Half done, abandoned in favor of browser-side host permission
 * checking. May be useful some time later, though.
 */

int
host_permission_p (char *string)
{
        /* This regexp is sufficient because the domain of host
         * permission checking is extremely restricted by the domain
         * of permissions checking. All the other match pattern
         * related things are managed by WebKit for us (are they?).
         */
        return g_regex_match_simple(".*://.*", string, 0, 0);
}

int
match_scheme (char *scheme, char *uri_scheme)
{
        if (!strcmp(scheme, "*")) {
                /* Star only matches http(s) and ws(s). */
                return (!strcmp("http", uri_scheme) ||
                        !strcmp("https", uri_scheme) ||
                        !strcmp("ws", uri_scheme) ||
                        !strcmp("wss", uri_scheme));
        } else {
                return !strcmp(scheme, uri_scheme);
        }
}

int
match_host (char *hostname, char *uri_hostname)
{
        if (!strcmp(hostname, "*"))
                return 1;
        else if (hostname[0] == '*' &&
                 hostname[1] == '.' &&
                 (strlen(hostname) - 2) < strlen(uri_hostname))
                return match_host(
                        (hostname+2),
                        (uri_hostname +
                         (strlen(uri_hostname) - strlen(hostname+2))));
        else
                return !strcmp(hostname, uri_hostname);
}

int
match_path (char *path, char *uri_path, char *uri_query)
{
        /* TODO: Write it. */
        return 1;
}

int
match_pattern_match (char *match_pattern, char *uri)
{
        /* FIXME: What is the maximum scheme length? */
        char *uri_scheme = g_malloc(sizeof(char) * 20);
        char *uri_host = g_malloc(sizeof(char) * 1000);
        char *uri_path = g_malloc(sizeof(char) * 10000);
        char *uri_query = g_malloc(sizeof(char) * 10000);
        /* FIXME: Maybe process the last arg (GError)? */
        g_uri_split(uri, 0,
                    &uri_scheme, NULL,
                    &uri_host, NULL,
                    &uri_path, &uri_query, NULL, NULL);
        char **tmp = g_strsplit(match_pattern, "://", 2);
        char *scheme = tmp[0];
        char **tmp2 = g_strsplit(*tmp, "/", 2);
        char *host = tmp2[0];
        char *path = tmp2[1];
        /* Testing scheme. */
        if (!match_scheme(scheme, uri_scheme))
                return 0;

        /* Testing hostname. */
        if (!match_host(host, uri_host))
                return 0;

        if (!match_path(path, uri_path, uri_query))
                return 0;

        return 1;
}

/** has_permission
 *
 * Check whether the extension (named extension_name) has requested
 * permission.
 */
int
has_permission (char* extension_name, char* permission)
{
        if (extension_name) {
                ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
                JSCValue *manifest = jsc_value_new_from_json(
                        jsc_context_new(), data->manifest);
                JSCValue *permissions = jsc_value_object_get_property(manifest, "permissions");
                int i;
                char **props = jsc_value_object_enumerate_properties(permissions);
                if (props)
                        for (i = 0; *(props+i) != NULL; ++i)
                                if (!strcmp(jsc_value_to_string(
                                                    jsc_value_object_get_property_at_index(
                                                            permissions, i)),
                                            permission))
                                        return 1;
                /*TODO: Match host permissions?
                 * Leave it to the browser side?
                 */
                return 0;
        }
        return 1;
}

/** empty_constructor_callback
 *
 * A utility constructor callback when you don't need any object to be created.
 */
void *
empty_constructor_callback (void)
{
        return NULL;
}

/** message_reply_and_save_callback
 *
 * The callback to use for the replies to all the WebKitUserMessages
 * sent to the browser. Parses the reply parameters and puts them into
 * the matching DATA cell.
 *
 * user_data should be a ulong ID cast to void pointer. The
 * approximate usage is:

 unsigned long int index = get_next_data_counter();
 webkit_web_page_send_message_to_view(
     PAGE, message, NULL, message_reply_and_save_callback,
     (void*) index);
 return index;

 * In the callback of the message-sending JS function inside the
 * Promise-returing API endpoint.
 *
 * This pattern is neatly encapsulated into the
 * SEND_MESSAGE_RETURN_ID, sou you'll be better off just using it.
 */
void
message_reply_and_save_callback (GObject *web_page,
                                 GAsyncResult *res,
                                 void *user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *contents = NULL;
        int* index = g_malloc(sizeof(unsigned long int));
        *index = (unsigned long int) user_data;
        if (params)
                contents = (char*) g_variant_get_string(params, NULL);
        if (contents)
                g_hash_table_insert(DATA, index, contents);
}

/** get_result
 *
 * Look the result up in DATA.
 * If there's a result and
 * - check_only is set, return the result.
 * - check_only is not set, return the result and remove it from
 *   DATA.
 */
JSCValue *
get_result (unsigned long int data_index, int check_only)
{
        JSCContext *context = jsc_context_get_current();
        char *data;
        if (data_index) {
                if (check_only) {
                        char *result = g_hash_table_lookup(
                                DATA, (void *) &data_index);
                        return jsc_value_new_boolean(context, (result? 1 : 0));
                }
                data = g_hash_table_lookup(DATA, (void *) &data_index);
                g_hash_table_remove(DATA, (void *) &data_index);
                if (data &&
                    !strncmp(ERROR_MESSAGE_PREFIX, data,
                             strlen(ERROR_MESSAGE_PREFIX)))
                        return jsc_value_constructor_call(
                                jsc_context_get_value(context, "Error"),
                                G_TYPE_STRING, (data + strlen(ERROR_MESSAGE_PREFIX)),
                                G_TYPE_NONE);
                return jsc_value_new_from_json(context, data);
        }
        return jsc_value_new_undefined(context);
}

/** todo_method_callback
 *
 * A small plug to set all the not-yet-implemented method callbacks to.
 *
 * In most cases, TODO_METHOD macro will do the thing you want.
 */
JSCValue *
todo_method_callback (GPtrArray *args, void *user_data)
{
        g_print("%s is not yet implemented.\n", (char *) user_data);
        return jsc_value_new_undefined(jsc_context_get_current());
}

/** todo_property_callback
 *
 * A small plug to set all the not-yet-implemented property callbacks to.
 *
 * In most cases, TODO_PROP macro will do the thing you want.
 *
 * Does not work at the moment.
 */
JSCValue *
todo_property_callback (void *instance, void *user_data)
{
        g_print("%s is not yet implemented.\n", (char *) user_data);
        return jsc_value_new_undefined(jsc_context_get_current());
}

void
promise_callback (JSCValue *success, JSCValue *failure, void *user_data)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *tmp = jsc_value_function_call(
                JSCEVAL(context, "browser.drain"),
                G_TYPE_ULONG, (unsigned long int) user_data,
                JSC_TYPE_VALUE, success,
                JSC_TYPE_VALUE, failure,
                JSC_TYPE_VALUE, jsc_value_new_undefined(context),
                G_TYPE_INT, BROWSER_REPLY_TIMEOUT,
                G_TYPE_NONE);
}

/** make_promise
 *
 * Creates a promise wrapping around the provided ID.
 *
 * This Promise waits for the result of the message with this ID. Once
 * the result is there, it runs the success callback.
 *
 * If the result is a JS Error object (constructed if you return
 * something starting with ERROR_MESSAGE_PREFIX), it will run failure
 * callback.
 */
JSCValue *
make_promise (JSCContext *context, unsigned long int id)
{
        JSCValue *promise_initializer = jsc_value_new_function(
                context, NULL, G_CALLBACK(promise_callback),
                (void *) id, NULL,
                G_TYPE_NONE, 2, JSC_TYPE_VALUE, JSC_TYPE_VALUE);
        return jsc_value_constructor_call(
                jsc_context_get_value(context, "Promise"),
                JSC_TYPE_VALUE, promise_initializer, G_TYPE_NONE);
}
