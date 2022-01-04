// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

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
browser_get_result_callback (unsigned long int index)
{
        return get_result(index, 0);
}

static JSCValue *
browser_check_result_callback (unsigned long int index)
{
        return get_result(index, 1);
}

/** inject_browser
 *
 * While similar to other inject_* functions, this one should be
 * invoked before anything else. It sets up the browser variable and
 * all the guts of our WebExtensions implementation -- browser.drain,
 * browser.getResult, browser.checkResult, and browser.replyMessage
 * (JS methods with C callbacks working with the fundamental
 * extensions data).*/
void
inject_browser (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Browser, "browser");

        MAKE_FN(context, "browser", "replyMessage", browser_reply_message_callback, NULL, G_TYPE_NONE, 2, G_TYPE_ULONG, JSC_TYPE_VALUE);
        MAKE_FN(context, "browser", "getResult", browser_get_result_callback, NULL, JSC_TYPE_VALUE, 1, G_TYPE_ULONG);
        MAKE_FN(context, "browser", "checkResult", browser_check_result_callback, NULL, JSC_TYPE_VALUE, 1, G_TYPE_ULONG);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "timeout",
                jsc_value_new_number(context, BROWSER_REPLY_TIMEOUT));
        /* TODO: Rewrite browser.drain in C with GTasks (is it even possible?) */
        BIND_FN(context, "browser", "drain",
                "function drain (index, success_fn, failure_fn, default_val, count) {\
    if (typeof(default_val)==='undefined') default_val = [];            \
    if (typeof(count)==='undefined') count = browser.timeout;           \
    var is_result = browser.checkResult(index);                         \
    if (is_result) {                                                    \
        var result = browser.getResult(index);                          \
        if (result instanceof Error)                                    \
            failure_fn(result);                                         \
        else                                                            \
            success_fn(result);                                         \
    } else {                                                            \
        if (count <= 0)                                                 \
            success_fn(default_val);                                    \
        else                                                            \
            setTimeout(() =>                                            \
                drain(index, success_fn, failure_fn, default_val, count - 10), \
                10);                                                    \
    }                                                                   \
}                                                                       \
                                                                        \
drain");
}
