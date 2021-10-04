#include "globals.h"
#include "lisp.h"

static unsigned long int
lisp_eval_callback (JSCValue *object)
{
        /* If the URI starts with the lisp: scheme, evaluate*/
        if (!strncmp("lisp:", webkit_web_page_get_uri(PAGE), 5)) {
                char *json = jsc_value_to_json(object, 0);
                GVariant *variant = g_variant_new("ms", json);
                WebKitUserMessage *message = webkit_user_message_new("lisp.eval", variant);
                SEND_MESSAGE_RETURN_ID(message, i);
        }
        return 0;
}

void inject_lisp_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Lisp, "lisp");

        MAKE_FN(context, lispEval, lisp_eval_callback, G_TYPE_ULONG, 1, JSC_TYPE_VALUE);

        BIND_FN(context, "lisp", "eval", "lisp.eval = function (form) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            browser.drain(lispEval(form), success, [], 5000);   \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
lisp.eval");

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "lisp",
                jsc_context_evaluate(context, "lisp", -1));
}
