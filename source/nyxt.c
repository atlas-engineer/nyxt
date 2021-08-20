#include <webkit2/webkit-web-extension.h>
#include "globals.h"
#include "extevent.h"
#include "browser.h"
#include "management.h"
#include "tabs.h"
#include "runtime.h"

static void
inject_apis (void* extension_name, void *data, void *user_data)
{
        inject_browser((char*) extension_name);
        inject_extevent_api((char*) extension_name);
        inject_management_api((char*) extension_name);
        inject_tabs_api((char*) extension_name);
        inject_runtime_api((char*) extension_name);
}

static gboolean
user_message_received (WebKitWebPage     *web_page,
                       WebKitUserMessage *message,
                       gpointer           user_data)
{
        const char *name = webkit_user_message_get_name(message);
        const char *contents = g_variant_get_string(webkit_user_message_get_parameters(message), NULL);
        if (!strcmp("message", name)){
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
                if (!(jsc_value_is_boolean(tmp) && !jsc_value_to_boolean(tmp))
                    && !(jsc_value_is_undefined(tmp))) {
                        g_object_ref(message);
                        g_hash_table_insert(MESSAGES, (void*) name, message);
                }
                else {
                        webkit_user_message_send_reply(
                                message, webkit_user_message_new(name, NULL));
                }

                return TRUE;
        } else if (!strcmp("injectAPIs", name)){
                inject_apis(contents, NULL, NULL);
                return TRUE;
        } else {
                WebKitUserMessage *reply = webkit_user_message_new(name, NULL);
                webkit_user_message_send_reply(message, reply);
                return TRUE;
        }
}

static void
document_loaded_callback (WebKitWebPage *web_page)
{
        g_hash_table_foreach(EXTENSIONS_DATA, inject_apis, NULL);
}

static void
add_extensions_reply_callback (GObject      *web_page,
                               GAsyncResult *res,
                               gpointer     user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) web_page, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        const char *extensions = g_variant_get_string(params, NULL);
        extensions_data_add_from_json(extensions);
}

static void
add_extensions (WebKitWebPage *web_page)
{
        WebKitUserMessage *message =
                webkit_user_message_new("listExtensions", NULL);
        webkit_web_page_send_message_to_view(web_page, message, NULL,
                                             add_extensions_reply_callback, NULL);
}

static void
web_page_created_callback (WebKitWebExtension *extension,
                           WebKitWebPage      *web_page,
                           gpointer            user_data)
{
        g_signal_connect (web_page, "document-loaded",
                          G_CALLBACK (document_loaded_callback),
                          NULL);
        g_signal_connect (web_page, "user-message-received",
                          G_CALLBACK (user_message_received),
                          NULL);
        PAGE = web_page;
        add_extensions(web_page);
        g_hash_table_foreach(EXTENSIONS_DATA, inject_apis, NULL);
}

G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
        MANAGEMENT = malloc(sizeof(Management));
        TABS = malloc(sizeof(Tabs));
        RUNTIME = malloc(sizeof(Runtime));

        EXTENSIONS_DATA = g_hash_table_new(g_str_hash, g_str_equal);
        MESSAGES = g_hash_table_new(g_str_hash, g_str_equal);
        IS_PRIVILEGED = 0;

        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
}
