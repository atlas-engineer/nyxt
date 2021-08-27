#include <webkit2/webkit-web-extension.h>
#include "globals.h"

static gboolean
user_message_received (WebKitWebPage     *web_page,
                       WebKitUserMessage *message,
                       gpointer           user_data)
{
        const char *name = webkit_user_message_get_name(message);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *contents = params ?
                (char *) g_variant_get_string(params, NULL) :
                NULL;
        if (!strcmp("message", name) && contents){
                JSCValue *meta = jsc_value_new_from_json(jsc_context_new(), contents);
                char *extension_name = jsc_value_to_string(jsc_value_object_get_property(meta, "extensionName"));
                JSCContext *context = get_extension_context(extension_name);
                meta = jsc_value_new_from_json(context, contents);
                JSCValue *sender = jsc_value_object_get_property(meta, "sender");
                JSCValue *object = jsc_value_object_get_property(meta, "message");
                char *code = malloc(sizeof(char*) * 10000);
                sprintf(code, "var p = browser.runtime.onMessage.run(JSON.parse('%s'), JSON.parse('%s'));\
if (p && p !== undefined) p.then((result) => browser.replyMessage('%s', result));\
p",
                        jsc_value_to_json(object, 0),
                        jsc_value_to_json(sender, 0),
                        name);
                JSCValue *tmp = jsc_context_evaluate(context, code, -1);
                if (tmp &&
                    JSC_IS_VALUE(tmp) &&
                    !(jsc_value_is_boolean(tmp) && !jsc_value_to_boolean(tmp)) &&
                    !(jsc_value_is_undefined(tmp))) {
                        g_object_ref(message);
                        g_hash_table_insert(MESSAGES, (void*) name, message);
                }
                else {
                        webkit_user_message_send_reply(
                                message, webkit_user_message_new(name, NULL));
                }
        } else if (!strcmp("injectAPIs", name) && contents){
                inject_apis((void *) contents, NULL, NULL);
        } else {
                WebKitUserMessage *reply = webkit_user_message_new(name, NULL);
                webkit_user_message_send_reply(message, reply);
        }
        return TRUE;
}

static void
web_page_created_callback (WebKitWebExtension *extension,
                           WebKitWebPage      *web_page,
                           gpointer            user_data)
{
        g_signal_connect (web_page, "user-message-received",
                          G_CALLBACK (user_message_received),
                          NULL);
        PAGE = web_page;
}

G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data
(WebKitWebExtension *extension, GVariant *user_data)
{
        init_global_objects();
        const char *json = g_variant_get_string(user_data, NULL);
        extensions_data_add_from_json(json);
        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
}
