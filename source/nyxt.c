#include <webkit2/webkit-web-extension.h>
#include "globals.h"
#include "browser.h"
#include "management.h"
#include "tabs.h"
#include "runtime.h"
#include <json-glib/json-glib.h>

static WebKitScriptWorld *hello_world = NULL;

static gboolean
user_message_received (WebKitWebPage     *web_page,
                       WebKitUserMessage *message,
                       gpointer           user_data)
{
        return FALSE;
}

static void
inject_apis (char* extension_name, ExtensionData *data, void *user_data)
{
        inject_browser(extension_name);
        inject_management_api(extension_name);
        inject_tabs_api(extension_name);
        inject_runtime_api(extension_name);
}

static void
document_loaded_callback (WebKitWebPage *web_page)
{
        g_hash_table_foreach (EXTENSIONS_DATA, inject_apis, NULL);
}

static void
add_extensions_reply_callback (GObject *web_page,
                                           GAsyncResult *res,
                                           gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) web_page, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        JsonParser *parser = json_parser_new();
        JsonNode *root;
        const char *extensions = g_variant_get_string(params, NULL);
        json_parser_load_from_data(parser, extensions, -1, NULL);
        root = json_parser_get_root(parser);
        extensions_data_add_from_json_root(root, (WebKitWebPage *) web_page);
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
}

G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
        MANAGEMENT = malloc(sizeof(Management));
        TABS = malloc(sizeof(Tabs));
        RUNTIME = malloc(sizeof(Runtime));

        EXTENSIONS_DATA = g_hash_table_new(g_str_hash, g_str_equal);

        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
}
