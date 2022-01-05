// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "management.h"

static JSCValue *
management_get_self_callback (void* extension_name)
{
        GVariant *variant = g_variant_new("ms", (char*) extension_name);
        WebKitUserMessage *message = webkit_user_message_new("management.getSelf", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), id);
}

void
inject_management_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Management, "management");

        /* TODO_PROP(Management, onInstalled); */
        /* TODO_PROP(Management, onUninstalled); */
        /* TODO_PROP(Management, onEnabled); */
        /* TODO_PROP(Management, onDisabled); */

        MAKE_FN(context, "management", "getSelf", management_get_self_callback, extension_name, JSC_TYPE_VALUE, 0, G_TYPE_NONE);

        TODO_METHOD(context, management, getAll);
        TODO_METHOD(context, management, get);
        TODO_METHOD(context, management, install);
        TODO_METHOD(context, management, uninstall);
        TODO_METHOD(context, management, uninstallSelf);
        TODO_METHOD(context, management, getPermissionWarningsById);
        TODO_METHOD(context, management, getPermissionWarningsByManifest);
        TODO_METHOD(context, management, setEnabled);
        TODO_METHOD(context, management, getAll);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "management",
                jsc_context_evaluate(context, "management", -1));
}
