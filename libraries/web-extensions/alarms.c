#include "globals.h"
#include "alarms.h"

void inject_alarms_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Alarms, "alarms");

        /* TODO_PROP(Alarms, onAlarm); */

        TODO_METHOD(context, alarms, clear);
        TODO_METHOD(context, alarms, clearAll);
        TODO_METHOD(context, alarms, create);
        TODO_METHOD(context, alarms, get);
        TODO_METHOD(context, alarms, getAll);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "alarms",
                jsc_context_evaluate(context, "alarms", -1));
}
