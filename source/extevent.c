#include "globals.h"
#include "extevent.h"

static Extevent *
extevent_constructor_callback ()
{
        Extevent *event = malloc(sizeof(Extevent));
        event->listeners = g_ptr_array_new();
        return event;
}

static void
extevent_free (Extevent *event)
{
        g_ptr_array_free(event->listeners, TRUE);
        free(event);
}

static void
extevent_add_listener_callback (Extevent *instance, JSCValue *callback, void *user_data)
{
        g_object_ref(callback);
        g_ptr_array_add(instance->listeners, callback);
}

static JSCValue *
extevent_has_listener_callback (Extevent *instance, JSCValue *callback, void *user_data)
{
        return jsc_value_new_boolean(
                jsc_context_get_current(),
                g_ptr_array_find(instance->listeners, callback, NULL));
}

static JSCValue *
extevent_has_listeners_callback (Extevent *instance, JSCValue *callback, void *user_data)
{
        return jsc_value_new_boolean(
                jsc_context_get_current(),
                instance->listeners->len);
}

static void
extevent_remove_listener_callback (Extevent *instance, JSCValue *callback, void *user_data)
{
        g_ptr_array_remove(instance->listeners, callback);
}

static JSCValue *
extevent_run_callback (Extevent *instance, GPtrArray *args, void *user_data)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *jsargs = jsc_value_new_array_from_garray(context, args);
        JSCValue *result = jsc_value_new_undefined(context);
        int i;
        if (instance->listeners->len) {
                for (i = 0; i < instance->listeners->len; i++) {
                        JSCValue *fn = instance->listeners->pdata[i];
                        JSCValue *tmp = jsc_value_function_call(
                                jsc_context_evaluate(
                                        context, "var apply = (fn, args) => fn(...args); apply", -1),
                                JSC_TYPE_VALUE, fn, JSC_TYPE_VALUE, jsargs,
                                G_TYPE_NONE);
                        if (!(jsc_value_is_boolean(tmp) && !jsc_value_to_boolean(tmp))
                            && !(jsc_value_is_undefined(tmp))) {
                                result = tmp;
                                break;
                        }
                }
        }
        return result;
}

void inject_extevent_api (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        JSCClass *ExtEvent = jsc_context_register_class(
                context, "ExtEvent", NULL, NULL, (GDestroyNotify) extevent_free);
        JSCValue *ExtEvent_constructor = jsc_class_add_constructor(
                ExtEvent, NULL, G_CALLBACK(extevent_constructor_callback),
                NULL, NULL, G_TYPE_POINTER, 0, G_TYPE_NONE);
        jsc_class_add_method(ExtEvent, "addListener",
                             G_CALLBACK(extevent_add_listener_callback),
                             NULL, NULL, G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        jsc_class_add_method(ExtEvent, "hasListener",
                             G_CALLBACK(extevent_has_listener_callback),
                             NULL, NULL, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        jsc_class_add_method(ExtEvent, "hasListeners",
                             G_CALLBACK(extevent_has_listeners_callback),
                             NULL, NULL, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        jsc_class_add_method(ExtEvent, "removeListener",
                             G_CALLBACK(extevent_remove_listener_callback),
                             NULL, NULL, G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        jsc_class_add_method_variadic(ExtEvent, "run",
                                      G_CALLBACK(extevent_run_callback),
                                      NULL, NULL, JSC_TYPE_VALUE);
        jsc_context_set_value(context, "ExtEvent", ExtEvent_constructor);
}
