#include "globals.h"
#include "storage.h"

JSCValue *
storage_local_set_callback (JSCValue *object, void *extension_id)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, (char *)extension_id));
        jsc_value_object_set_property(wrapper, "keys", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("storage.local.set", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

JSCValue *
storage_local_remove_callback (JSCValue *object, void *extension_id)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        jsc_value_object_set_property(wrapper, "keys", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("storage.local.remove", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

JSCValue *
storage_local_clear_callback (void *extension_id)
{
        GVariant *variant = g_variant_new("ms", extension_id);
        WebKitUserMessage *message = webkit_user_message_new("storage.local.clear", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

JSCValue *
storage_local_get_callback (JSCValue *object, void *extension_id)
{

        JSCContext *context = jsc_context_get_current();
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        jsc_value_object_set_property(wrapper, "keys", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("storage.local.get", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

void
inject_storage_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        char *extension_id = get_extension_id(extension_name);
        MAKE_CLASS(context, Storage, "storage");

        /* TODO_PROP(Storage, sync); */
        /* TODO_PROP(Storage, managed); */

        jsc_context_set_value(context, "local", jsc_value_new_object(context, NULL, NULL));

        MAKE_FN(context, "local", "get", storage_local_get_callback, extension_id, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        MAKE_FN(context, "local", "set", storage_local_set_callback, extension_id, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        MAKE_FN(context, "local", "remove", storage_local_remove_callback, extension_id, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        MAKE_FN(context, "local", "clear", storage_local_clear_callback, extension_id, JSC_TYPE_VALUE, 0, G_TYPE_NONE);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "storage", -1), "local",
                jsc_context_evaluate(context, "local", -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "storage",
                jsc_context_evaluate(context, "storage", -1));
}
