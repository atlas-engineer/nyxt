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
management_get_self_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
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
        JSCContext *context = get_extension_context(extension_name);
        MAKE_CLASS(context, Management, "management");
        JSCValue *managementGetSelf = jsc_value_new_function(
                context, "managementGetSelf",
                G_CALLBACK(management_get_self_callback), NULL, NULL,
                G_TYPE_NONE, 1, G_TYPE_STRING);
        JSCValue *managementGetSelfResult = jsc_value_new_function(
                context, "getSelfResult",
                G_CALLBACK(management_get_self_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        char *management_get_self_js = malloc(sizeof(char) * 800);
        jsc_context_set_value(context, "managementGetSelf", managementGetSelf);
        jsc_context_set_value(context, "managementGetSelfResult", managementGetSelfResult);
        sprintf(management_get_self_js, "management.getSelf = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            managementGetSelf(\"%s\");                                  \
            setTimeout(() => success(managementGetSelfResult()), 0); \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
management.getSelf", extension_name);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "management", -1),
                "getSelf",
                jsc_context_evaluate(context, management_get_self_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "management",
                jsc_context_evaluate(context, "management", -1));
        free(management_get_self_js);
}
