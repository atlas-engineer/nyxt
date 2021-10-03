#include "globals.h"
#include "storage.h"

void
storage_set_callback (char *storage_area, char *extension_id, JSCValue *object)
{
        if (!strcmp("local", storage_area)){
                JSCContext *context = jsc_context_get_current();
                JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                        wrapper, "extensionId",
                        jsc_value_new_string(context, extension_id));
                jsc_value_object_set_property(wrapper, "keys", object);
                char *json = jsc_value_to_json(wrapper, 0);
                GVariant *variant = g_variant_new("ms", json);
                WebKitUserMessage *message = webkit_user_message_new("storage.local.set", variant);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

void
storage_remove_callback (char *storage_area, char *extension_id, JSCValue *object)
{
        if (!strcmp("local", storage_area)){
                JSCContext *context = jsc_context_get_current();
                JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                        wrapper, "extensionId",
                        jsc_value_new_string(context, extension_id));
                jsc_value_object_set_property(wrapper, "keys", object);
                char *json = jsc_value_to_json(wrapper, 0);
                GVariant *variant = g_variant_new("ms", json);
                WebKitUserMessage *message = webkit_user_message_new("storage.local.remove", variant);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

void
storage_clear_callback (char *storage_area, char *extension_id)
{
        if (!strcmp("local", storage_area)){
                GVariant *variant = g_variant_new("ms", extension_id);
                WebKitUserMessage *message = webkit_user_message_new("storage.local.clear", variant);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

unsigned long int
storage_get_callback (char *storage_area, char *extension_id, JSCValue *object)
{
        if (!strcmp("local", storage_area)){
                JSCContext *context = jsc_context_get_current();
                JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                        wrapper, "extensionId",
                        jsc_value_new_string(context, extension_id));
                jsc_value_object_set_property(wrapper, "keys", object);
                char *json = jsc_value_to_json(wrapper, 0);
                GVariant *variant = g_variant_new("ms", json);
                WebKitUserMessage *message = webkit_user_message_new("storage.local.get", variant);
                SEND_MESSAGE_RETURN_ID(message, i);
        }
        return 0;
}

void
inject_storage_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Storage, "storage");

        /* TODO_PROP(Storage, sync); */
        /* TODO_PROP(Storage, managed); */

        MAKE_FN(context, storageGet, storage_get_callback, G_TYPE_ULONG, 3, G_TYPE_STRING, G_TYPE_STRING, JSC_TYPE_VALUE);
        MAKE_FN(context, storageSet, storage_set_callback, G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_STRING, JSC_TYPE_VALUE);
        MAKE_FN(context, storageRemove, storage_remove_callback, G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_STRING, JSC_TYPE_VALUE);
        MAKE_FN(context, storageClear, storage_clear_callback, G_TYPE_NONE, 2, G_TYPE_STRING, G_TYPE_STRING);

        jsc_context_set_value(context, "local", jsc_value_new_object(context, NULL, NULL));

        BIND_FN(context, "local", "get", "function get (keys) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            browser.drain(storageGet(\"local\", runtime.id, keys), success, undefined, 5000);  \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    });                                                                 \
};                                                                      \
                                                                        \
get")
        BIND_FN(context, "local", "set", "function set (keys) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageSet(\"local\", runtime.id, keys);                    \
            setTimeout(() => success(),                                 \
                           10);                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    });                                                                 \
};                                                                      \
                                                                        \
set");
        BIND_FN(context, "local", "remove", "function remove (keys) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageRemove(\"local\", runtime.id, keys);                 \
            setTimeout(() => success(),                                 \
                           10);                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    });                                                                 \
};                                                                      \
                                                                        \
remove");
        BIND_FN(context, "local", "clear", "function clear () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageClear(\"local\", runtime.id);                        \
            setTimeout(() => success(),                                 \
                           10);                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    });                                                                 \
};                                                                      \
                                                                        \
clear");
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "storage", -1), "local",
                jsc_context_evaluate(context, "local", -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "storage",
                jsc_context_evaluate(context, "storage", -1));
}
