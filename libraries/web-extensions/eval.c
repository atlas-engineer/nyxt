#include "globals.h"
#include "eval.h"

static unsigned long int
browser_eval_callback (JSCValue *object)
{
        /* If the URI starts with the privileged scheme, evaluate. */
        if (!strncmp(PRIVILEGED_SCHEME,
                     webkit_web_page_get_uri(PAGE),
                     strlen(PRIVILEGED_SCHEME))) {
                char *json = jsc_value_to_json(object, 0);
                GVariant *variant = g_variant_new("ms", json);
                WebKitUserMessage *message = webkit_user_message_new("browser.eval", variant);
                SEND_MESSAGE_RETURN_ID(message, i);
        }
        return 0;
}

void inject_eval_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);

        MAKE_FN(context, browserEval, browser_eval_callback, G_TYPE_ULONG, 1, JSC_TYPE_VALUE);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "eval",
                JSCEVAL(context, "browser.eval = function (form) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            browser.drain(browserEval(form), success); \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
browser.eval"));
}
