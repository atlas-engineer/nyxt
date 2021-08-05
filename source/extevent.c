#include "globals.h"
#include "extevent.h"

JSCClass *ExtEvent;

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
                *(instance->listeners->pdata) == NULL);
}

static void
extevent_remove_listener_callback (Extevent *instance, JSCValue *callback, void *user_data)
{
        g_ptr_array_remove(instance->listeners, callback);
}

static void
extevent_run_callback (Extevent *instance, GPtrArray *args, void *user_data)
{
        JSCValue **fn;
        JSCValue *tmp;
        if (*(instance->listeners->pdata))
                for (fn = (JSCValue **)(instance->listeners->pdata);
                     *fn != NULL; fn++)
                        tmp = jsc_value_function_callv(
                                *fn, args->len, (JSCValue **)args->pdata);
}

void inject_extevent_api (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        ExtEvent = jsc_context_register_class(
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
                                      NULL, NULL, G_TYPE_NONE);
        jsc_context_set_value(context, "ExtEvent", ExtEvent_constructor);
}
