#include "globals.h"
#include "browser.h"

static void
browser_reply_message_callback (unsigned long int message_id, JSCValue *result)
{
        GVariant *reply_contents = g_variant_new_string(
                jsc_value_to_json(result, 0));
        WebKitUserMessage *reply = webkit_user_message_new("message", reply_contents);
        webkit_user_message_send_reply(g_hash_table_lookup(DATA, &message_id), reply);
}

static JSCValue *
browser_get_result_callback (int index)
{
        return get_result(index);
}

void
inject_browser (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Browser, "browser");
        MAKE_FN(context, browserReplyMessage, browser_reply_message_callback, G_TYPE_NONE, 2, G_TYPE_ULONG, JSC_TYPE_VALUE);
        MAKE_FN(context, browserGetResult, browser_get_result_callback, JSC_TYPE_VALUE, 1, G_TYPE_ULONG);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "replyMessage",
                browserReplyMessage);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "getResult",
                browserGetResult);
        BIND_FN(context, "browser", "drain", "function drain (index, success_fn, default_val, count, ...args) {\
    var res = browser.getResult(index);                                \
    if (res) {                                                          \
        success_fn(res);                                                \
    } else {                                                            \
        if (count <= 0)                                                 \
            success_fn(default_val);                                    \
        else                                                            \
            setTimeout(() =>                                            \
                drain(index, success_fn, default_val, count - 10, ...args), \
                10);                                                    \
    }                                                                   \
}                                                                       \
                                                                        \
drain");
}
