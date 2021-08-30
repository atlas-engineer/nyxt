#include "globals.h"
#include "extension.h"

void inject_extension_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);

        jsc_context_set_value(context, "extension",
                              jsc_value_new_object(context, NULL, NULL));

        BIND_FN(context, "extension", "getURL", "extension.getURL = function (string) {\
    return runtime.getURL(string);                                      \
};                                                                      \
                                                                        \
extension.getURL");
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "extension",
                jsc_context_evaluate(context, "extension", -1));
}
