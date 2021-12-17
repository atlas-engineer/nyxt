#include "globals.h"
#include "web_navigation.h"

void inject_web_navigation_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, WebNavigation, "webNavigation");

        /* TODO_PROP(WebNavigation, onBeforeNavigate); */
        /* TODO_PROP(WebNavigation, onCommited); */
        /* TODO_PROP(WebNavigation, onDOMContentLoaded); */
        /* TODO_PROP(WebNavigation, onCompleted); */
        /* TODO_PROP(WebNavigation, onErrorOccured); */
        /* TODO_PROP(WebNavigation, onCreatedNavigationTarget); */
        /* TODO_PROP(WebNavigation, onReferenceFragmentUpdated); */
        /* TODO_PROP(WebNavigation, onTabReplaced); */
        /* TODO_PROP(WebNavigation, onHistoryStateUpdated); */

        TODO_METHOD(context, web_navigation, getFrame);
        TODO_METHOD(context, web_navigation, getAllFrames);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "webNavigation",
                jsc_context_evaluate(context, "web_navigation", -1));
}
