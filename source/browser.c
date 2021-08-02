#include "globals.h"
#include "browser.h"

static void
inject_browser (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        JSCClass *Browser = jsc_context_register_class(context, "Browser", NULL, NULL, NULL);
        JSCValue *Browser_constructor = jsc_class_add_constructor(
                Browser, NULL, G_CALLBACK(empty_constructor_callback),
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);
        jsc_context_set_value(context, "browser", jsc_value_new_object(context, NULL, Browser));
}
