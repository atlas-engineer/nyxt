// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "extevent.h"
#include "runtime.h"

static JSCValue *
runtime_send_message_callback (GPtrArray *args, void* extension_name)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *extension_id;
        JSCValue *payload;
        if (args->len == 1) {
                payload = args->pdata[0];
                extension_id = jsc_value_new_string(
                        context, get_extension_id((char *) extension_name));
        } else if (args->len == 2 &&
                   jsc_value_object_has_property(
                           args->pdata[1], "includeTlsChannelId") &&
                   jsc_value_is_undefined(
                           jsc_value_object_get_property_at_index(args->pdata[1], 1))) {
                payload = args->pdata[0];
                extension_id = jsc_value_new_string(
                        context, get_extension_id((char *) extension_name));
        } else if (args->len == 2 || args->len == 3) {
                extension_id = args->pdata[0];
                payload = args->pdata[1];
        } /* TODO: Return error if no parse case matched. */
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        jsc_value_object_set_property(wrapper, "extensionId", extension_id);
        jsc_value_object_set_property(wrapper, "message", payload);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("runtime.sendMessage", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, id);
}

static JSCValue *
runtime_get_manifest_callback (void *extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, (char *)extension_name);
        return jsc_value_new_from_json(jsc_context_get_current(), data->manifest);
}

static JSCValue *
runtime_get_platform_info_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("runtime.getPlatformInfo", NULL);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

static JSCValue *
runtime_get_browser_info_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("runtime.getBrowserInfo", NULL);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

char *
runtime_get_url_callback (char *path, void* extension_name)
{
        JSCContext *context = jsc_context_get_current();
        char *solid_path = path + (path[0] == '/' ? 1 : 0);
        char *result = g_malloc(sizeof(char) * 4000000000); /* 4GB */
        result = "data:text/html,<h1>Resource not found</h1>";
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, (char *)extension_name);
        if (!data)
                return result;
        JSCValue *files = data->files;
        char **properties = jsc_value_object_enumerate_properties(files);
        char **property;
        if (properties)
                for (property = properties; *property != NULL; property++){
                        if (!strcmp(*property, solid_path))
                                result = jsc_value_to_string(jsc_value_object_get_property(files, *property));
                }
        return result;
}

void inject_runtime_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Runtime, "runtime");
        /* TODO_PROP(Runtime, lastError); */
        /* TODO_PROP(Runtime, onStartup); */
        /* TODO_PROP(Runtime, onInstalled); */
        /* TODO_PROP(Runtime, onSuspend); */
        /* TODO_PROP(Runtime, onSuspendCanceled); */
        /* TODO_PROP(Runtime, onUpdateAvailable); */
        /* TODO_PROP(Runtime, onBrowserUpdateAvailable); */
        /* TODO_PROP(Runtime, onConnect); */
        /* TODO_PROP(Runtime, onConnectExternal); */
        /* TODO_PROP(Runtime, onMessageExternal); */
        /* TODO_PROP(Runtime, onRestartRequired); */

        MAKE_FNV(context, "runtime", "sendMessage", runtime_send_message_callback, extension_name);
        MAKE_FN(context, "runtime", "getManifest", runtime_get_manifest_callback, extension_name, JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        MAKE_FN(context, "runtime", "getPlatformInfo", runtime_get_platform_info_callback, NULL, JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        MAKE_FN(context, "runtime", "getBrowserInfo", runtime_get_browser_info_callback, NULL, JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        MAKE_FN(context, "runtime", "getURL", runtime_get_url_callback, extension_name, G_TYPE_STRING, 1, G_TYPE_STRING);

        MAKE_EVENT(context, "runtime", "onMessage");

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "runtime", -1), "id",
                jsc_value_new_string(context, get_extension_id(extension_name)));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "runtime", -1), "name",
                jsc_value_new_string(context, extension_name));
        TODO_METHOD(context, runtime, getBackGroundPage);
        TODO_METHOD(context, runtime, openOptionsPage);
        TODO_METHOD(context, runtime, setUninstallURL);
        TODO_METHOD(context, runtime, reload);
        TODO_METHOD(context, runtime, requestUpdateCheck);
        TODO_METHOD(context, runtime, connect);
        TODO_METHOD(context, runtime, connectNative);
        TODO_METHOD(context, runtime, sendNativeMessage);
        TODO_METHOD(context, runtime, getPackageDirectoryEntry);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "runtime",
                jsc_context_evaluate(context, "runtime", -1));
}
