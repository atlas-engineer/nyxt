#include "globals.h"
#include "commands.h"

void inject_commands_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Commands, "commands");

        /* TODO_PROP(Commands, onCommand); */

        TODO_METHOD(context, commands, getAll);
        TODO_METHOD(context, commands, reset);
        TODO_METHOD(context, commands, update);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "commands",
                jsc_context_evaluate(context, "commands", -1));
}
