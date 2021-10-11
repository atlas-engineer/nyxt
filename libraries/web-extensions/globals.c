#include "globals.h"

WebKitWebPage *PAGE;

WebKitWebExtension *EXTENSION;

GHashTable *EXTENSIONS_DATA;

GHashTable *MESSAGES;

int IS_PRIVILEGED;

unsigned long int DATA_COUNTER;

GHashTable *DATA;

unsigned long int
get_next_data_counter ()
{
        return ++DATA_COUNTER;
}

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
                        JSCValue *manifest = jsc_value_object_get_property_at_index(data, 1);
                        JSCValue *files = jsc_value_object_get_property_at_index(data, 3);
                        char *id = jsc_value_to_string(
                                jsc_value_object_get_property_at_index(data, 0));
                        int is_privileged = jsc_value_to_int32(jsc_value_object_get_property_at_index(data, 2));
                        WebKitScriptWorld *world = webkit_script_world_new_with_name(name);
                        if (is_privileged && !IS_PRIVILEGED) {
                                IS_PRIVILEGED = is_privileged;
                                world = webkit_script_world_get_default();
                        }
                        extension = malloc(sizeof(ExtensionData));
                        extension->name = (char*) name;
                        extension->manifest = manifest;
                        extension->files = files;
                        extension->extension_id = id;
                        extension->world = world;
                        g_hash_table_insert(EXTENSIONS_DATA, (void*) name, extension);
                }
        }
}

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

JSCContext *
get_extension_context (char* extension_name)
{
        WebKitScriptWorld *world = get_extension_world(extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        return webkit_frame_get_js_context_for_script_world(frame, world);
}

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

/* int */
/* host_permission_p (char *string) */
/* { */
/*         /\* This regexp is sufficient because the domain of host */
/*          * permission checking is extremely restricted by the domain */
/*          * of permissions checking. All the other match pattern */
/*          * related things are managed by WebKit for us (are they?). */
/*          *\/ */
/*         return g_regex_match_simple(".*://.*", string, 0, 0); */
/* } */

/* int */
/* match_host (char *hostname, char *uri_hostname) */
/* { */
/*         if (!strcmp(hostname, "*")) */
/*                 return 1; */
/*         if (*hostname == '*' && *(hostname+1) == '.') */
/*                 for */
/*         for (i = 0, cu = *uri_hostname, cmp = mp_hostname;; i++, cu++, cmp++) */
/*                 if (cmp == '*') */
/*                         for() */
/* } */

/* int */
/* match_pattern_match (char *match_pattern, char *uri) */
/* { */
/*         char *uri_scheme = g_uri_parse_scheme(uri); */
/*         char **uri_hostname = malloc(sizeof(char) * 1000); */
/*         char *uri_path = g_filename_from_uri(uri, hostname, NULL); */
/*         char **mp_parts = g_strsplit(host_permission, "://", 2); */
/*         /\* Testing scheme. It can be either a full scheme or a star. */
/*          * */
/*          * If both strcmp-s return non-zero, then it's neither the */
/*          * same scheme as the uri, nor the star. */
/*          *\/ */
/*         if(strcmp(*mp_parts, "*") && */
/*            strcmp(*mp_parts, uri_scheme)) */
/*                 return 0; */

/*         /\* Testing hostname. */
/*          * */
/*          * Hostname is anything before '?' or '/'. If the string */
/*          * starts with '/', then there's no hostname. */
/*          *\/ */

/*         char **mp_remains = g_strsplit(*(mp_parts+1), "/", 2); */
/*         int has_hostname = !(**(mp_parts+1) == '/'); */
/*         char *mp_hostname = has_hostname ? *mp_remains : ""; */
/*         char *mp_path = *(mp_remains+(has_hostname?1:0)); */
/*         char *cu, *cmp; */
/*         int i; */
/*         if(!match_host(mp_hostname, uri_hostname)) */
/*                 return 0; */
/* } */

int
has_permission (char* extension_name, char* permission)
{
        if (extension_name) {
                ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
                JSCValue *manifest = data->manifest;
                JSCValue *permissions = jsc_value_object_get_property(manifest, "permissions");
                JSCValue *prop;
                int i;
                char *prop_name;
                char **props = jsc_value_object_enumerate_properties(permissions);
                for (i = 0, prop_name = *(props+i);
                     prop_name != NULL;
                     ++i, prop_name = *(props+i)) {
                        if (jsc_value_is_undefined(prop = jsc_value_object_get_property_at_index(
                                                           permissions, i)))
                                return 0;
                        if (!strcmp(jsc_value_to_string(prop), permission))
                                return 1;
                        /*TODO: Match host permissions?
                         * Leave it to the browser side?
                         */
                }
        }
        return 0;
}

void *
empty_constructor_callback (void)
{
        return NULL;
}

void
message_reply_and_save_callback (GObject *web_page,
                                 GAsyncResult *res,
                                 void *user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *contents = NULL;
        int* index = malloc(sizeof(unsigned long int));
        *index = (unsigned long int) user_data;
        if (params)
                contents = (char*) g_variant_get_string(params, NULL);
        if (contents)
                g_hash_table_insert(DATA, index, contents);
}

JSCValue *
get_result (unsigned long int data_index, int check_only)
{
        JSCContext *context = jsc_context_get_current();
        char *data;
        if (check_only)
                return jsc_value_new_boolean(
                        context, (int) g_hash_table_lookup (
                                DATA, (void *) &data_index));
        if (data_index) {
                data = g_hash_table_lookup(DATA, (void *) &data_index);
                g_hash_table_remove(DATA, (void *) &data_index);
                return jsc_value_new_from_json(context, data);
        }
        return jsc_value_new_undefined(context);
}


JSCValue *
todo_method_callback (GPtrArray *args, void *user_data)
{
        g_print("%s is not yet implemented.", (char *) user_data);
        return jsc_value_new_undefined(jsc_context_get_current());
}

JSCValue *
todo_property_callback (void *instance, void *user_data)
{
        g_print("%s is not yet implemented.", (char *) user_data);
        return jsc_value_new_undefined(jsc_context_get_current());
}
