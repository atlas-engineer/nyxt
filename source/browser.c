#include "globals.h"
#include "browser.h"

static void
browser_reply_message_callback (char *message_name, JSCValue *result)
{
        GVariant *reply_contents = g_variant_new_string(
                jsc_value_to_json(result, 0));
        WebKitUserMessage *reply = webkit_user_message_new(message_name, reply_contents);
        webkit_user_message_send_reply(g_hash_table_lookup(MESSAGES, message_name), reply);
}

void
inject_browser (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        MAKE_CLASS(context, Browser, "browser");
        MAKE_FN(context, browserReplyMessage, browser_reply_message_callback, G_TYPE_NONE, 2, G_TYPE_STRING, JSC_TYPE_VALUE);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "replyMessage",
                browserReplyMessage);
        BIND_FN(context, "browser", "drain", "function drain (result_fn, success_fn, default_val, count, ...args) {\
    var res = result_fn(...args);                                       \
    if (res) {                                                          \
        success_fn(res);                                                \
    } else {                                                            \
        if (count === 0)                                                \
            success_fn(default_val);                                    \
        else                                                            \
            setTimeout(() =>                                            \
                drain(result_fn, success_fn, default_val, count - 1, ...args), \
                1000);                                                  \
    }                                                                   \
}                                                                       \
                                                                        \
drain");
}
