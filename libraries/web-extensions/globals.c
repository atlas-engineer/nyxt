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
get_result (unsigned long int data_index)
{
        JSCContext *context = jsc_context_get_current();
        char *data;
        if (data_index) {
                data = g_hash_table_lookup(DATA, (void *) &data_index);
                g_hash_table_remove(DATA, (void *) &data_index);
                return jsc_value_new_from_json(context, data);
        }
        return jsc_value_new_null(context);
}


JSCValue *
not_yet_implemented(GPtrArray *args, void *user_data)
{
        g_print("JS API for %s is not yet implemented.", (char *) user_data);
        return jsc_value_new_undefined(jsc_context_get_current());
}
