// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "permissions.h"

void inject_permissions_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Permissions, "permissions");

        /* TODO_PROP(Permissions, onAdded); */
        /* TODO_PROP(Permissions, onRemoved); */

        TODO_METHOD(context, permissions, contains);
        TODO_METHOD(context, permissions, getAll);
        TODO_METHOD(context, permissions, remove);
        TODO_METHOD(context, permissions, request);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "permissions",
                jsc_context_evaluate(context, "permissions", -1));
}
