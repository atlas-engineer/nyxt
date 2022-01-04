// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "runtime.h"
#include "extension.h"

void inject_extension_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Extension, "extension");

        /* TODO_PROP(Extension, lastError); */
        /* TODO_PROP(Extension, inIncognitoContext); */
        /* TODO_PROP(Extension, onRequest); */
        /* TODO_PROP(Extension, onRequestExternal); */

        MAKE_FN(context, "extension", "getURL", runtime_get_url_callback, extension_name, G_TYPE_STRING, 1, G_TYPE_STRING);

        TODO_METHOD(context, extension, getBackgroundPage);
        TODO_METHOD(context, extension, getExtensionTabs); /* DEPRECATED */
        TODO_METHOD(context, extension, getViews);
        TODO_METHOD(context, extension, isAllowedIncognitoAccess);
        TODO_METHOD(context, extension, isAllowedFileSchemeAccess);
        TODO_METHOD(context, extension, sendRequest);
        TODO_METHOD(context, extension, setUpdateUrlData);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "extension",
                jsc_context_evaluate(context, "extension", -1));
}
