#include "globals.h"
#include "history.h"

void inject_history_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, History, "history");

        /* TODO_PROP(History, onTitleChanged); */
        /* TODO_PROP(History, onVisited); */
        /* TODO_PROP(History, onVisitRemoved); */

        TODO_METHOD(context, history, search);
        TODO_METHOD(context, history, getVisits);
        TODO_METHOD(context, history, addUrl);
        TODO_METHOD(context, history, deleteUrl);
        TODO_METHOD(context, history, deleteRange);
        TODO_METHOD(context, history, deleteAll);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "history",
                jsc_context_evaluate(context, "history", -1));
}
