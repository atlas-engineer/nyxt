#include "globals.h"

WebKitWebPage *PAGE;

GHashTable *EXTENSIONS_DATA;

GHashTable *MESSAGES;

WebKitWebExtension *EXTENSION;

void
extensions_data_add_from_json(const char *json)
{
        JSCContext *dummy_context = jsc_context_new();
        JSCValue *object = jsc_value_new_from_json(dummy_context, json);
        char **properties = jsc_value_object_enumerate_properties(object);
        char **property;
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        for (property = properties; *property != NULL; property++){
                ExtensionData *extension;
                const char *name = *property;
                char *ext_json = jsc_value_to_string(
                        jsc_value_object_get_property(
                                object, *property));
                JSCValue* manifest = jsc_value_new_from_json(dummy_context, ext_json);
                extension = malloc(sizeof(ExtensionData));
                extension->name = (char*) name;
                extension->manifest = manifest;
                extension->world = webkit_script_world_new_with_name(name);
                g_hash_table_insert(EXTENSIONS_DATA, (void*) name, extension);
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
        char *contents = (char*) g_variant_get_string(params, NULL);
        char **place = (char **) user_data;
        if (contents)
                *(place) = contents;
}
