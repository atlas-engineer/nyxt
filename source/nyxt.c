#include <webkit2/webkit-web-extension.h>
#include "globals.h"
#include "extevent.h"
#include "browser.h"
#include "management.h"
#include "tabs.h"
#include "runtime.h"
#include "storage.h"

static void
inject_apis (void* extension_name, void *data, void *user_data)
{
        inject_browser((char*) extension_name);
        inject_extevent_api((char*) extension_name);
        inject_management_api((char*) extension_name);
        inject_tabs_api((char*) extension_name);
        inject_runtime_api((char*) extension_name);
        inject_storage_api((char *) extension_name);
}

static void
window_object_cleared_callback (WebKitScriptWorld *world,
                                WebKitWebPage     *web_page,
                                WebKitFrame       *frame,
                                gpointer           user_data)
{
        inject_apis(user_data, NULL, NULL);
}

static void
set_window_object_cleared (void *key, void *value, void *user_data)
{
        ExtensionData *data = (ExtensionData *) value;
        /* We only inject APIs for the privileged extension
         * (owning a default world), if there's one. */
        if (IS_PRIVILEGED && webkit_script_world_get_default() == data->world ||
            !IS_PRIVILEGED)
                g_signal_connect (data->world, "window-object-cleared",
                                  G_CALLBACK(window_object_cleared_callback),
                                  key);
}

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
        extensions_data_add_from_json(user_data);
        g_hash_table_foreach(EXTENSIONS_DATA, set_window_object_cleared, NULL);
}

G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data
(WebKitWebExtension *extension, GVariant *user_data)
{
        MANAGEMENT = malloc(sizeof(Management));
        TABS = malloc(sizeof(Tabs));
        RUNTIME = malloc(sizeof(Runtime));
        STORAGE = malloc(sizeof(Storage));

        EXTENSIONS_DATA = g_hash_table_new(g_str_hash, g_str_equal);
        MESSAGES = g_hash_table_new(g_str_hash, g_str_equal);
        IS_PRIVILEGED = 0;

        const char *json = g_variant_get_string(user_data, NULL);
        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          (void *) json);
}
