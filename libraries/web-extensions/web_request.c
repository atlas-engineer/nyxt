#include "globals.h"
#include "web_request.h"

GHashTable *REQUESTS;

gboolean send_request_callback (WebKitWebPage     *web_page,
                                WebKitURIRequest  *request,
                                WebKitURIResponse *redirected_response,
                                gpointer           user_data)
{
        char *extension_name = (char *) user_data;
        JSCContext *context = get_extension_context(extension_name);
        unsigned long numeric_request_id = get_next_data_counter();
        char *request_id = g_strdup_printf("%lu", numeric_request_id);
        JSCValue *details = jsc_value_new_object(context, NULL, NULL);
        char *document_url =
                (char *) ((!strcmp(webkit_web_page_get_uri(web_page),
                                   webkit_uri_request_get_uri(request))) ?
                          NULL : webkit_web_page_get_uri(web_page));
        ExtensionData *extension_data = g_hash_table_lookup(EXTENSIONS_DATA,
                                                            extension_name);
        char *tab_id = extension_data ? extension_data->tab_id : NULL;
        /* TODO: cookieStoreId, proxyInfo, type/resourceType, thirdParty, urlClassification. */
        jsc_value_object_set_property(
                details, "documentUrl",
                document_url ? jsc_value_new_string(context, document_url)
                : jsc_value_new_undefined(context));
        /* FIXME: Same as documentUrl due to frame ignorance. */
        jsc_value_object_set_property(
                details, "originUrl",
                document_url ? jsc_value_new_string(context, document_url)
                : jsc_value_new_undefined(context));
        /* FIXME: Always the same. We should fetch frame IDs somehow. */
        jsc_value_object_set_property(details, "frameId",
                                      jsc_value_new_number(context, 0));
        jsc_value_object_set_property(details, "parentFrameId",
                                      jsc_value_new_number(context, -1));
        /* FIXME: Always false.
         * Maybe ask the browser for which page is incognito? */
        jsc_value_object_set_property(details, "incognito",
                                      jsc_value_new_boolean(context, 0));
        if (webkit_uri_request_get_http_method(request))
                jsc_value_object_set_property(
                        details, "method",
                        jsc_value_new_string(
                                context, webkit_uri_request_get_http_method(
                                        request)));
        jsc_value_object_set_property(details, "requestId",
                                      jsc_value_new_string(context, request_id));
        if (tab_id)
                jsc_value_object_set_property(details, "tabId",
                                              jsc_value_new_from_json(
                                                      context, (const char *) tab_id));
        jsc_value_object_set_property(details, "timestamp",
                                      jsc_value_new_number(
                                              context, ((double) time(0)) * 1000));
        jsc_value_object_set_property(
                details, "url",
                jsc_value_new_string(
                        context, webkit_uri_request_get_uri(request)));
        g_hash_table_insert(REQUESTS, request_id, request);
        return FALSE;
}

void inject_web_request_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, WebRequest, "webRequest");

        MAKE_EVENT(context, "webRequest", "onBeforeRequest");
        MAKE_EVENT(context, "webRequest", "onBeforeSendHeaders");
        MAKE_EVENT(context, "webRequest", "onSendHeaders");
        MAKE_EVENT(context, "webRequest", "onHeadersReceived");
        MAKE_EVENT(context, "webRequest", "onAuthRequired");
        MAKE_EVENT(context, "webRequest", "onResponseStarted");
        MAKE_EVENT(context, "webRequest", "onBeforeRedirect");
        MAKE_EVENT(context, "webRequest", "onCompleted");
        MAKE_EVENT(context, "webRequest", "onErrorOccured");

        TODO_METHOD(context, web_request, handlerBehaviorChanged);
        TODO_METHOD(context, web_request, filterResponseData);
        TODO_METHOD(context, web_request, getSecurityInfo);

        g_signal_connect (PAGE, "send-request",
                          G_CALLBACK(send_request_callback),
                          extension_name);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "webRequest",
                jsc_context_evaluate(context, "web_request", -1));
}
