// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include <glib-2.0/glib.h>
#include <webkit2/webkit-web-extension.h>
#include "globals.h"
#include "extevent.h"
#include "browser.h"
/* API headers with a sole inject_*_api function. */
#include "management.h"
#include "tabs.h"
#include "runtime.h"
#include "extension.h"
#include "storage.h"
#include "alarms.h"
#include "bookmarks.h"
#include "browser_action.h"
#include "commands.h"
#include "history.h"
#include "notifications.h"
#include "permissions.h"
#include "web_navigation.h"
#include "web_request.h"

/** inject_apis
 *
 * Injects all the APIs extension (named by extension_name) has the
 * access to.
 *
 * If you implement a new API, add more inject_*_api functions there,
 * including those from the respective headers.
 *
 * data and user_data are not used.
 */
static void
inject_apis (void* extension_name, void *data, void *user_data)
{
        inject_browser((char*) extension_name);
        inject_extevent_api((char*) extension_name);
        if (has_permission(extension_name, "management"))
                inject_management_api((char*) extension_name);
        inject_tabs_api((char*) extension_name);
        inject_runtime_api((char*) extension_name);
        inject_extension_api((char*) extension_name);
        if (has_permission(extension_name, "storage"))
                inject_storage_api((char *) extension_name);
        if (has_permission(extension_name, "alarms"))
                inject_alarms_api((char *) extension_name);
        if (has_permission(extension_name, "bookmarks"))
                inject_bookmarks_api((char *) extension_name);
        inject_browser_action_api((char *) extension_name);
        inject_commands_api((char *) extension_name);
        if (has_permission(extension_name, "history"))
                inject_history_api((char *) extension_name);
        if (has_permission(extension_name, "notifications"))
                inject_notifications_api((char *) extension_name);
        inject_permissions_api((char *) extension_name);
        if (has_permission(extension_name, "webNavigation"))
                inject_web_navigation_api((char *) extension_name);
        if (has_permission(extension_name, "webRequest"))
                inject_web_request_api((char *) extension_name);
}

/** window_object_cleared_callback
 *
 * A callback for "window-object-cleared" signal.
 *
 * Is used to inject JS APIs as early as possible in the page loading
 * process, as WebExtensions are quire demanding about time there.
 *
 * user_data should be a string with extension name.
 */
static void
window_object_cleared_callback (WebKitScriptWorld *world,
                                WebKitWebPage     *web_page,
                                WebKitFrame       *frame,
                                gpointer           user_data)
{
        inject_apis(user_data, NULL, NULL);
}

/** set_window_object_cleared
 *
 * A helper to set "window-object-cleared" callback for a particular
 * extension.
 *
 * key and value suggest usage as a hash table (EXTENSIONS_DATA)
 * iterator and are the name of the extension and ExtensionData
 * describing the extension resrectively.
 *
 * user_data is not used.
 */
static void
set_window_object_cleared (void *key, void *value, void *user_data)
{
        ExtensionData *data = (ExtensionData *) value;
        /* We only inject APIs for the privileged extension
         * (owning a default world), if there's one. */
        if (!data->is_injected &&
            ((IS_PRIVILEGED &&
              webkit_script_world_get_default() == data->world) ||
             !IS_PRIVILEGED)) {
                g_signal_connect (data->world, "window-object-cleared",
                                  G_CALLBACK(window_object_cleared_callback),
                                  key);
                data->is_injected = 1;
        }
}

/** user_message_received_callback
 *
 * A callback for "user-message-received" signal.
 *
 * Process the messages that the browser sends to the PAGE. Those are
 * used for two things now:
 * - messages (e.g., runtime.sendMessages).
 * - API inhjection in the atypical scenarios -- send the
 *   "injectAPIs"-named message to trigger API injection.
 */
static gboolean
user_message_received_callback (WebKitWebPage     *web_page,
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
                unsigned long int id = get_next_data_counter();
                JSCValue *tmp = jsc_value_function_call(
                        jsc_context_evaluate(
                                context, "var run  = (object, sender, id) => {\
var p = browser.runtime.onMessage.run(object, sender);                  \
if (p && p !== undefined)                                               \
        p.then((result) => browser.replyMessage(id, result));    \
return p;                                                               \
};                                                                      \
                                                                        \
run", -1),
                        JSC_TYPE_VALUE, object, JSC_TYPE_VALUE, sender,
                        G_TYPE_ULONG, id,
                        G_TYPE_NONE);
                if (tmp &&
                    JSC_IS_VALUE(tmp) &&
                    !(jsc_value_is_boolean(tmp) && !jsc_value_to_boolean(tmp)) &&
                    !(jsc_value_is_undefined(tmp))) {
                        g_object_ref(message);
                        unsigned long int *key = g_malloc(sizeof(unsigned long int));
                        *key = id;
                        g_hash_table_insert(DATA, (void *) key, message);
                }
                else {
                        webkit_user_message_send_reply(
                                message, webkit_user_message_new(name, NULL));
                }
        } else if (!strcmp("injectAPIs", name) && contents){
                extensions_data_add_from_json(contents);
                g_hash_table_foreach(EXTENSIONS_DATA, set_window_object_cleared, NULL);
                WebKitUserMessage *reply = webkit_user_message_new(name, NULL);
                webkit_user_message_send_reply(message, reply);
        } else {
                WebKitUserMessage *reply = webkit_user_message_new(name, NULL);
                webkit_user_message_send_reply(message, reply);
        }
        return TRUE;
}

/** web_page_created_callback
 *
 * A callback for "page-created" signal.
 *
 * Set PAGE to the created page, as there's unlikely to even be more
 * than one page per WebKitWebExtension.
 *
 * Add a "user-message-received" signal callback.
 *
 * Process the extensions data (passed from
 * webkit_web_extension_initialize_with_user_data) and adds callbacks
 * for the generation of necessary APIs for those.
 */
static void
web_page_created_callback (WebKitWebExtension *extension,
                           WebKitWebPage      *web_page,
                           gpointer            user_data)
{
        g_signal_connect (web_page, "user-message-received",
                          G_CALLBACK (user_message_received_callback),
                          NULL);
        PAGE = web_page;
        /* webkit_web_page_send_message_to_view( */
        /*         PAGE, webkit_user_message_new("ready", g_variant_new("ms", "")), NULL, NULL, NULL); */
}

/** webkit_web_extension_initialize_with_user_data
 *
 * A usual entry point for any WebKit extension with user data.
 * See README for what the data format is.
 *
 * Initialize several global variables (from global.h) and adds a
 * "page-created" signal callback.
 *
 * Print "The WebExtensions support library is loaded" when done.
 */
G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
        EXTENSIONS_DATA = g_hash_table_new(g_str_hash, g_str_equal);
        IS_PRIVILEGED = 0;
        DATA_COUNTER = 0;
        DATA = g_hash_table_new_full(g_int64_hash, g_int64_equal,
                                        (GDestroyNotify) free, NULL);
        REQUESTS = g_hash_table_new(g_str_hash, g_str_equal);

        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
        g_print("The WebExtensions support library is loaded\n");
}
