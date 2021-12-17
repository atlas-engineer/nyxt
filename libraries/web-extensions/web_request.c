#include "globals.h"
#include "web_request.h"

void inject_web_request_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, WebRequest, "webRequest");

        /* TODO_PROP(WebRequest, onBeforeRequest); */
        /* TODO_PROP(WebRequest, onBeforeSendHeaders); */
        /* TODO_PROP(WebRequest, onSendHeaders); */
        /* TODO_PROP(WebRequest, onHeadersReceived); */
        /* TODO_PROP(WebRequest, onAuthRequired); */
        /* TODO_PROP(WebRequest, onResponseStarted); */
        /* TODO_PROP(WebRequest, onBeforeRedirect); */
        /* TODO_PROP(WebRequest, onCompleted); */
        /* TODO_PROP(WebRequest, onErrorOccured); */

        TODO_METHOD(context, web_request, handlerBehaviorChanged);
        TODO_METHOD(context, web_request, filterResponseData);
        TODO_METHOD(context, web_request, getSecurityInfo);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "webRequest",
                jsc_context_evaluate(context, "web_request", -1));
}
