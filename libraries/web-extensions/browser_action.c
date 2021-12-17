#include "globals.h"
#include "browser_action.h"

void inject_browser_action_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, BrowserAction, "browser_action");

        /* TODO_PROP(BrowserAction, onClicked); */

        TODO_METHOD(context, browser_action, setTitle);
        TODO_METHOD(context, browser_action, getTitle);
        TODO_METHOD(context, browser_action, setIcon);
        TODO_METHOD(context, browser_action, setPopup);
        TODO_METHOD(context, browser_action, getPopup);
        TODO_METHOD(context, browser_action, openPopup);
        TODO_METHOD(context, browser_action, setBadgeText);
        TODO_METHOD(context, browser_action, getBadgeText);
        TODO_METHOD(context, browser_action, setBadgeBacgroundColor);
        TODO_METHOD(context, browser_action, getBadgeBacgroundColor);
        TODO_METHOD(context, browser_action, setBadgeTextColor);
        TODO_METHOD(context, browser_action, getBadgeTextColor);
        TODO_METHOD(context, browser_action, enable);
        TODO_METHOD(context, browser_action, disable);
        TODO_METHOD(context, browser_action, isEnabled);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "browserAction",
                jsc_context_evaluate(context, "browser_action", -1));
}
