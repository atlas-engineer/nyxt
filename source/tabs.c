#include "globals.h"
#include "tabs.h"

Tabs *TABS;

static void
tabs_query_reply_callback (GObject *web_page,
                           GAsyncResult *res,
                           gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "[]";
        TABS->tabs = (char *) json;
}

static JSCValue *
tabs_query_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
        return jsc_value_new_from_json(context, TABS->tabs);
}

static void
tabs_query_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.queryObject", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_query_reply_callback, NULL);
}

static void
tabs_create_reply_callback (GObject *web_page,
                           GAsyncResult *res,
                           gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "{}";
        TABS->tab = (char *) json;
}

static JSCValue *
tabs_create_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
        return jsc_value_new_from_json(context, TABS->tab);
}

static void
tabs_create_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.createProperties", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_create_reply_callback, NULL);
}

static void
tabs_get_current_reply_callback (GObject *web_page,
                                 GAsyncResult *res,
                                 gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "{}";
        TABS->tab = (char *) json;
}

static JSCValue *
tabs_get_current_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
        return jsc_value_new_from_json(context, TABS->tab);
}

static void
tabs_get_current_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.getCurrent", NULL);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_get_current_reply_callback, NULL);
}

static void
tabs_get_reply_callback (GObject *web_page,
                         GAsyncResult *res,
                         gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "null";
        TABS->tab = (char *) json;
}

static JSCValue *
tabs_get_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
        return jsc_value_new_from_json(context, TABS->tab);
}

static void
tabs_get_callback (int id)
{
        char *num = malloc(sizeof(char) * 1000);
        sprintf(num, "%d", id);
        GVariant *variant = g_variant_new("s", num);
        WebKitUserMessage *message = webkit_user_message_new("tabs.get", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_get_reply_callback, NULL);
}

static void
tabs_print_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.print", NULL);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
}

static void
tabs_send_message_reply_callback (GObject *web_page,
                                  GAsyncResult *res,
                                  gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "undefined";
        TABS->reply = (char *) json;
}

static JSCValue *
tabs_send_message_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
        return jsc_value_new_from_json(context, TABS->reply);
}

static void
tabs_send_message_callback (char *extension_id, double tab_id, JSCValue *object)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        jsc_value_object_set_property(
                wrapper, "tabId",
                jsc_value_new_number(context, tab_id));
        jsc_value_object_set_property(wrapper, "message", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.sendMessage", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_send_message_reply_callback, NULL);
}

static void
tabs_insert_css_callback (char *extension_id, int tab_id, JSCValue *object)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        jsc_value_object_set_property(
                wrapper, "tabId",
                jsc_value_new_number(context, tab_id));
        jsc_value_object_set_property(wrapper, "css", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.insertCSS", variant);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
}

void
inject_tabs_api (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        MAKE_CLASS(context, Tabs, "tabs");
        JSCValue *tabsQuery = jsc_value_new_function(
                context, "tabsQuery",
                G_CALLBACK(tabs_query_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        JSCValue *tabsQueryResult = jsc_value_new_function(
                context, "tabsQueryResult",
                G_CALLBACK(tabs_query_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        JSCValue *tabsCreate = jsc_value_new_function(
                context, "tabsCreate",
                G_CALLBACK(tabs_create_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        JSCValue *tabsCreateResult = jsc_value_new_function(
                context, "tabsCreateResult",
                G_CALLBACK(tabs_create_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        JSCValue *tabsGetCurrent = jsc_value_new_function(
                context, "tabsGetCurrent",
                G_CALLBACK(tabs_get_current_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCValue *tabsGetCurrentResult = jsc_value_new_function(
                context, "tabsGetCurrentResult",
                G_CALLBACK(tabs_get_current_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        JSCValue *tabsGet = jsc_value_new_function(
                context, "tabsGet",
                G_CALLBACK(tabs_get_callback), NULL, NULL,
                G_TYPE_NONE, 1, G_TYPE_INT);
        JSCValue *tabsGetResult = jsc_value_new_function(
                context, "tabsGetResult",
                G_CALLBACK(tabs_get_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        JSCValue *tabsSendMessage = jsc_value_new_function(
                context, "tabsSendMessage",
                G_CALLBACK(tabs_send_message_callback), NULL, NULL,
                G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_DOUBLE, JSC_TYPE_VALUE);
        JSCValue *tabsSendMessageResult = jsc_value_new_function(
                context, "tabsSendMessageResult",
                G_CALLBACK(tabs_send_message_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        JSCValue *print = jsc_value_new_function(
                context, NULL, G_CALLBACK(tabs_print_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCValue *tabsInsertCSS = jsc_value_new_function(
                context, NULL, G_CALLBACK(tabs_insert_css_callback), NULL, NULL,
                G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT, JSC_TYPE_VALUE);
        jsc_context_set_value(context, "tabsQuery", tabsQuery);
        jsc_context_set_value(context, "tabsQueryResult", tabsQueryResult);
        jsc_context_set_value(context, "tabsCreate", tabsCreate);
        jsc_context_set_value(context, "tabsCreateResult", tabsCreateResult);
        jsc_context_set_value(context, "tabsGetCurrent", tabsGetCurrent);
        jsc_context_set_value(context, "tabsGetCurrentResult", tabsGetCurrentResult);
        jsc_context_set_value(context, "tabsGet", tabsGet);
        jsc_context_set_value(context, "tabsGetResult", tabsGetResult);
        jsc_context_set_value(context, "tabsSendMessage", tabsSendMessage);
        jsc_context_set_value(context, "tabsSendMessageResult", tabsSendMessageResult);
        jsc_context_set_value(context, "tabsInsertCSS", tabsInsertCSS);
        char *tabs_query_js = "tabs.query = function (queryObject) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsQuery(queryObject);                                     \
            setTimeout(() =>                                            \
                success(tabsQueryResult()), 0);                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.query",
                *tabs_create_js = "tabs.create = function (createProperties) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsCreate(createProperties);                               \
            setTimeout(() =>                                            \
                success(tabsCreateResult()), 0);                        \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.create",
                *tabs_get_current_js = "tabs.getCurrent = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsGetCurrent();                                           \
            setTimeout(() =>                                            \
                success(tabsGetCurrentResult()), 0);                    \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.getCurrent",
                *tabs_get_js = "tabs.get = function (getProperties) {\
    return new Promise(function (success, failure) {                 \
        try {                                                        \
            tabsGet(getProperties);                                  \
            setTimeout(() => {                                       \
                var result = tabsGetResult();                        \
                if (result)                                          \
                    success(result);                                 \
                else                                                 \
                    throw new Error(\"No tab found!\");},            \
                       0);                                           \
            );                                                       \
        } catch (error) {                                            \
            return failure(error);                                   \
        };                                                           \
    });                                                              \
};                                                                   \
                                                                     \
tabs.get",
                *tabs_send_message_js = "tabs.sendMessage = function (tabId, message, options) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            management.getSelf().then(function (info) {                 \
            tabsSendMessage(info.id, tabId, message);                   \
            setTimeout(() => {                                          \
                success(runtimeSendMessageResult());},                  \
                        10);});                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.sendMessage",
                *tabs_insert_css_js = "tabs.insertCSS = function (one, two) {\
    var tabId = (two === undefined) ? 0 : one;                          \
    var css = (two === undefined) ? one : two;                          \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            management.getSelf().then(function (info) {                 \
                tabsInsertCSS(info.id, tabId, css);                     \
                setTimeout(() => success(),                             \
                           10);});                                      \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.insertCSS";
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "query",
                jsc_context_evaluate(context, tabs_query_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "create",
                jsc_context_evaluate(context, tabs_create_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "getCurrent",
                jsc_context_evaluate(context, tabs_get_current_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "get",
                jsc_context_evaluate(context, tabs_get_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "sendMessage",
                jsc_context_evaluate(context, tabs_send_message_js, -1));
        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                "print", print);
        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                "insertCSS",
                jsc_context_evaluate(context, tabs_insert_css_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "tabs",
                jsc_context_evaluate(context, "tabs", -1));
}
