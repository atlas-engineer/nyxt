#include "globals.h"
#include "management.h"

static unsigned long int
management_get_self_callback (char *extension_name)
{
        GVariant *variant = g_variant_new("ms", extension_name);
        WebKitUserMessage *message = webkit_user_message_new("management.getSelf", variant);
        SEND_MESSAGE_RETURN_ID(message, i);
}

void
inject_management_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Management, "management");
        MAKE_FN(context, managementGetSelf, management_get_self_callback, G_TYPE_ULONG, 1, G_TYPE_STRING);
        char *management_get_self_js = malloc(sizeof(char) * 800);
        sprintf(management_get_self_js, "management.getSelf = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            browser.drain(managementGetSelf(\"%s\"), success, {}, 5000);\
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
