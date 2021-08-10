#include <webkit2/webkit-web-extension.h>
#include "globals.h"
#include "extevent.h"
#include "browser.h"
#include "management.h"
#include "tabs.h"
#include "runtime.h"

static WebKitScriptWorld *hello_world = NULL;

static gboolean
user_message_received (WebKitWebPage     *web_page,
                       WebKitUserMessage *message,
                       gpointer           user_data)
{
        const char *name = webkit_user_message_get_name(message);
        const char *contents = g_variant_get_string(webkit_user_message_get_parameters(message), NULL);
        g_print("Got message. Name is %s, contents is %s\n", name, contents);
        if (!strcmp("message", name)){
                g_print("Message has \"message\" type\n");
                JSCValue *meta = jsc_value_new_from_json(jsc_context_new(), contents);
                g_print("Got metadata\n");
                char *extension_name = jsc_value_to_string(jsc_value_object_get_property(meta, "extensionName"));
                g_print("Got extension name\n");
                JSCContext *context = get_extension_context(extension_name);
                g_print("Got context\n");
                meta = jsc_value_new_from_json(context, contents);
                g_print("Re-initialized metadata\n");
                JSCValue *sender = jsc_value_object_get_property(meta, "sender");
                g_print("Got sender\n");
                JSCValue *object = jsc_value_object_get_property(meta, "message");
                g_print("Got message object\n");
                char *code = malloc(sizeof(char*) * 10000);
                sprintf(code, "var p = browser.runtime.onMessage.run(JSON.parse('%s'), JSON.parse('%s'));\
if (p) p.then((result) => browser.replyMessage('%s', result));\
p",
                        jsc_value_to_json(object, 0),
                        jsc_value_to_json(sender, 0),
                        name);
                g_print("Printed the code: %s\n", code);
                JSCValue *result = jsc_context_evaluate(context, code, -1);
                g_print("Evaluated the code, result is %s\n", jsc_value_to_string(result));
                g_object_ref(message);
                g_hash_table_insert(MESSAGES, (void*) name, message);
                return TRUE;
        } else {
                g_print("Message has a type other than \"message\"\n");
                WebKitUserMessage *reply = webkit_user_message_new(name, NULL);
                g_print("Made the reply\n");
                webkit_user_message_send_reply(message, reply);
                g_print("Sent the reply\n");
                return TRUE;
        }
}

static void
inject_apis (void* extension_name, void *data, void *user_data)
{
        g_print("Injecting APIs for page %lu, URL %s",
                webkit_web_page_get_id(PAGE), webkit_web_page_get_uri(PAGE));
        inject_browser((char*) extension_name);
        inject_extevent_api((char*) extension_name);
        inject_management_api((char*) extension_name);
        inject_tabs_api((char*) extension_name);
        inject_runtime_api((char*) extension_name);
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

        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
}
