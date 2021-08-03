#include "globals.h"
#include "browser.h"

void
inject_browser (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        MAKE_CLASS(context, Browser, "browser");
}
