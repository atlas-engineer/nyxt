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
        return get_result(index, 0);
}

static JSCValue *
browser_check_result_callback (int index)
{
        return get_result(index, 1);
}

void
inject_browser (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Browser, "browser");

        MAKE_FN(context, browserReplyMessage, browser_reply_message_callback, G_TYPE_NONE, 2, G_TYPE_ULONG, JSC_TYPE_VALUE);
        MAKE_FN(context, browserGetResult, browser_get_result_callback, JSC_TYPE_VALUE, 1, G_TYPE_ULONG);
        MAKE_FN(context, browserCheckResult, browser_check_result_callback, JSC_TYPE_VALUE, 1, G_TYPE_ULONG);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "replyMessage",
                browserReplyMessage);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "getResult",
                browserGetResult);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "checkResult",
                browserCheckResult);
        BIND_FN(context, "browser", "drain", "function drain (index, success_fn, default, count) {\
    var is_result = browser.checkResult(index);                         \
    if (is_result) {                                                    \
        success_fn(browser.getResult(index));                           \
    } else {                                                            \
        if (count <= 0)                                                 \
            success_fn(default);                                    \
        else                                                            \
            setTimeout(() =>                                            \
                drain(index, success_fn, default_val, count - 10),      \
                10);                                                    \
    }                                                                   \
}                                                                       \
                                                                        \
drain");
}
