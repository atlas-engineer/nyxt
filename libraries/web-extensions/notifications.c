// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "notifications.h"

void inject_notifications_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Notifications, "notifications");

        /* TODO_PROP(Notifications, onButtonClicked); */
        /* TODO_PROP(Notifications, onClicked); */
        /* TODO_PROP(Notifications, onClosed); */
        /* TODO_PROP(Notifications, onShown); */

        TODO_METHOD(context, notifications, clear);
        TODO_METHOD(context, notifications, create);
        TODO_METHOD(context, notifications, getAll);
        TODO_METHOD(context, notifications, update);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "notifications",
                jsc_context_evaluate(context, "notifications", -1));
}
