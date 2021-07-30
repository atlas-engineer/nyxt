#include "globals.h"
#include "management.h"

Management *MANAGEMENT;

static void
management_get_self_reply_callback (GObject *web_page,
                                    GAsyncResult *res,
                                    gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "{}";
        MANAGEMENT->info = (char *) json;
}

static JSCValue *
management_get_self_result_callback (char *extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        return jsc_value_new_from_json(context, MANAGEMENT->info);
}

static void
management_get_self_callback (char *extension_name)
{
        GVariant *variant = g_variant_new("s", extension_name);
        WebKitUserMessage *message = webkit_user_message_new("management.getSelf", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, management_get_self_reply_callback, NULL);
}

void
inject_management_api (char* extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        g_print("Data is %p\n", data);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        g_print("Frame is %p\n", frame);
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        g_print("Context is %p\n", context);
        JSCValue *managementGetSelf = jsc_value_new_function(
                context, "managementGetSelf",
                G_CALLBACK(management_get_self_callback), NULL, NULL,
                G_TYPE_NONE, 1, G_TYPE_STRING);
        g_print("ManagementGetSelf created\n");
        JSCValue *managementGetSelfResult = jsc_value_new_function(
                context, "getSelfResult",
                G_CALLBACK(management_get_self_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        g_print("ManagementGetSelfResult created\n");
        JSCClass *Management = jsc_context_register_class(context, "Management", NULL, NULL, NULL);
        g_print("management registered: %p\n", Management);
        JSCValue *Management_constructor = jsc_class_add_constructor(
                Management, NULL, G_CALLBACK(empty_constructor_callback),
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);
        g_print("Management constructor made: %p\n", Management_constructor);
        char *management_get_self_js = malloc(sizeof(char) * 900);
        jsc_context_set_value(context, "Management", Management_constructor);
        g_print("Management set\n");
        jsc_context_set_value(context, "managementGetSelf", managementGetSelf);
        g_print("ManagementGetSelf set\n");
        jsc_context_set_value(context, "managementGetSelfResult", managementGetSelfResult);
        g_print("ManagementGetSelfResult set\n");
        jsc_context_set_value(context, "management",
                              jsc_value_new_object(context, NULL, Management));
        g_print("management set\n");
        sprintf(management_get_self_js, "management.getSelf = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            managementGetSelf(\"%s\");                                  \
            setTimeout(() => success(managementGetSelfResult(\"%s\")), 20); \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
management.getSelf", extension_name, extension_name);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "management", -1),
                "getSelf",
                jsc_context_evaluate(context, management_get_self_js, -1));
        g_print("management.getSelf evaluated\n");
        free(management_get_self_js);
}
