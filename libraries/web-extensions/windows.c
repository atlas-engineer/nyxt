// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "windows.h"

void inject_windows_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Windows, "windows");

        MAKE_EVENT(context, "windows", "onCreated");
        MAKE_EVENT(context, "windows", "onRemoved");
        MAKE_EVENT(context, "windows", "onFocusChanged");

        jsc_value_object_set_property(JSCEVAL(context, "windows"),
                                      "WINDOW_ID_NONE",
                                      jsc_value_new_number(context, 0));
        jsc_value_object_set_property(JSCEVAL(context, "windows"),
                                      "WINDOW_ID_CURRENT",
                                      jsc_value_new_number(context, -1));

        TODO_METHOD(context, windows, get);
        TODO_METHOD(context, windows, getCurrent);
        TODO_METHOD(context, windows, getLastFocused);
        TODO_METHOD(context, windows, getAll);
        TODO_METHOD(context, windows, create);
        TODO_METHOD(context, windows, update);
        TODO_METHOD(context, windows, remove);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "windows",
                jsc_context_evaluate(context, "windows", -1));
}
