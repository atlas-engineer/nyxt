#include "globals.h"
#include "browser.h"

static void
browser_reply_message_callback (char *message_name, JSCValue *result)
{
        GVariant *reply_contents = g_variant_new_string(
                jsc_value_to_json(result, 0));
        g_print("Made the payload: %s\n", jsc_value_to_json(result, 0));
        WebKitUserMessage *reply = webkit_user_message_new(message_name, reply_contents);
        g_print("Made the reply\n");
        webkit_user_message_send_reply(g_hash_table_lookup(MESSAGES, message_name), reply);
        g_print("Sent the reply\n");
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
}
