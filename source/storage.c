#include "globals.h"
#include "storage.h"

Storage *STORAGE;

void
storage_set_callback (char *storage_area, JSCValue *object)
{
        if (!strcmp("local", storage_area)){
                GVariant *variant = g_variant_new("s", jsc_value_to_json(object, 0));
                WebKitUserMessage *message = webkit_user_message_new("storage.local.set", variant);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

void
storage_remove_callback (char *storage_area, JSCValue *object)
{
        if (!strcmp("local", storage_area)){
                GVariant *variant = g_variant_new("s", jsc_value_to_json(object, 0));
                WebKitUserMessage *message = webkit_user_message_new("storage.local.remove", variant);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

void
storage_clear_callback (char *storage_area)
{
        if (!strcmp("local", storage_area)){
                WebKitUserMessage *message = webkit_user_message_new("storage.local.set", NULL);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

void
storage_get_callback (char *storage_area, JSCValue *object)
{
        if (!strcmp("local", storage_area)){
                GVariant *variant = g_variant_new("s", jsc_value_to_json(object, 0));
                WebKitUserMessage *message = webkit_user_message_new("storage.local.get", variant);
                webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
        }
}

void
inject_storage_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Storage, "storage");

        MAKE_FN(context, storageGet, storage_get_callback, G_TYPE_NONE, 2, G_TYPE_STRING, JSC_TYPE_VALUE);
        MAKE_RESULT_FN(context, storageGetResult, &STORAGE->data);
        MAKE_FN(context, storageSet, storage_set_callback, G_TYPE_NONE, 2, G_TYPE_STRING, JSC_TYPE_VALUE);
        MAKE_FN(context, storageRemove, storage_remove_callback, G_TYPE_NONE, 2, G_TYPE_STRING, JSC_TYPE_VALUE);
        MAKE_FN(context, storageClear, storage_clear_callback, G_TYPE_NONE, 1, G_TYPE_STRING);

        jsc_context_set_value(context, "local", jsc_value_new_object(context, NULL, NULL));

        BIND_FN(context, "local", "get", "get = function (keys) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageGet(\"local\", keys);                                \
            browser.drain(storageGetResult, success, undefined, 5000);  \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    }                                                                   \
};                                                                      \
                                                                        \
get")
        BIND_FN(context, "local", "set", "set = function (keys) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageSet(\"local\", keys);                                \
            setTimeout(() => success(),                                 \
                           10);                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    }                                                                   \
};                                                                      \
                                                                        \
set");
        BIND_FN(context, "local", "remove", "remove = function (keys) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageRemove(\"local\", keys);                             \
            setTimeout(() => success(),                                 \
                           10);                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    }                                                                   \
};                                                                      \
                                                                        \
remove");
        BIND_FN(context, "local", "clear", "clear = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            storageClear(\"local\");                                    \
            setTimeout(() => success(),                                 \
                           10);                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        }                                                               \
    }                                                                   \
};                                                                      \
                                                                        \
clear");
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "storage", -1), "local",
                jsc_value_new_object(context, NULL, NULL));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "storage",
                jsc_context_evaluate(context, "storage", -1));
}
