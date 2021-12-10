#include "globals.h"
#include "tabs.h"

Tabs *TABS;

static void
tabs_query_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.query", variant);
        TABS->tabs = NULL;
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, message_reply_and_save_callback, &TABS->tabs);
}

static void
tabs_create_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.create", variant);
        TABS->tab = NULL;
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, message_reply_and_save_callback, &TABS->tab);
}

static void
tabs_get_current_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.getCurrent", NULL);
        TABS->tab = NULL;
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, message_reply_and_save_callback, &TABS->tab);
}

static void
tabs_get_callback (int id)
{
        char *num = malloc(sizeof(char) * 1000);
        sprintf(num, "%d", id);
        GVariant *variant = g_variant_new("s", num);
        WebKitUserMessage *message = webkit_user_message_new("tabs.get", variant);
        TABS->tab = NULL;
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, message_reply_and_save_callback, &TABS->tab);
}

static void
tabs_print_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.print", NULL);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
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
        TABS->reply = NULL;
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, message_reply_and_save_callback, &TABS->reply);
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

static void
tabs_remove_css_callback (char *extension_id, int tab_id, JSCValue *object)
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
        WebKitUserMessage *message = webkit_user_message_new("tabs.removeCSS", variant);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
}

static void
tabs_execute_script_callback (char *extension_id, int tab_id, JSCValue *object)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        jsc_value_object_set_property(
                wrapper, "tabId",
                jsc_value_new_number(context, tab_id));
        jsc_value_object_set_property(wrapper, "script", object);
                char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.executeScript", variant);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
}

void
inject_tabs_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Tabs, "tabs");

        MAKE_FN(context, tabsQuery, tabs_query_callback, G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        MAKE_RESULT_FN(context, tabsQueryResult, &TABS->tabs);
        MAKE_FN(context, tabsCreate, tabs_create_callback, G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        MAKE_RESULT_FN(context, tabsCreateResult, &TABS->tab);
        MAKE_FN(context, tabsGetCurrent, tabs_get_current_callback, G_TYPE_NONE, 0, G_TYPE_NONE);
        MAKE_RESULT_FN(context, tabsGetCurrentResult, &TABS->tab);
        MAKE_FN(context, tabsGet, tabs_get_callback, G_TYPE_NONE, 1, G_TYPE_INT);
        MAKE_RESULT_FN(context, tabsGetResult, &TABS->tab);
        MAKE_FN(context, tabsSendMessage, tabs_send_message_callback, G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_DOUBLE, JSC_TYPE_VALUE);
        MAKE_RESULT_FN(context, tabsSendMessageResult, &TABS->reply);
        MAKE_FN(context, print, tabs_print_callback, G_TYPE_NONE, 0, G_TYPE_NONE);
        MAKE_FN(context, tabsInsertCSS, tabs_insert_css_callback, G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT, JSC_TYPE_VALUE);
        MAKE_FN(context, tabsRemoveCSS, tabs_remove_css_callback, G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT, JSC_TYPE_VALUE);
        MAKE_FN(context, tabsExecuteScript, tabs_execute_script_callback, G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT, JSC_TYPE_VALUE);

        BIND_FN(context, "tabs", "query", "tabs.query = function (queryObject) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsQuery(queryObject);                                     \
            browser.drain(tabsQueryResult, success, [], 5000);            \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.query");
        BIND_FN(context, "tabs", "create", "tabs.create = function (createProperties) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsCreate(createProperties);                               \
            browser.drain(tabsCreateResult, success, {}, 5000);           \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.create");
        BIND_FN(context, "tabs", "getCurrent", "tabs.getCurrent = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsGetCurrent();                                           \
            browser.drain(tabsGetCurrentResult, success, {}, 5000);       \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.getCurrent");
        BIND_FN(context, "tabs", "get", "tabs.get = function (getProperties) {\
    return new Promise(function (success, failure) {                 \
        try {                                                        \
            tabsGet(getProperties);                                  \
            browser.drain(tabsGetResult, success, {}, 5000);           \
            );                                                       \
        } catch (error) {                                            \
            return failure(error);                                   \
        };                                                           \
    });                                                              \
};                                                                   \
                                                                     \
tabs.get");
        BIND_FN(context, "tabs", "sendMessage", "tabs.sendMessage = function (tabId, message, options) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            management.getSelf().then(function (info) {                 \
                tabsSendMessage(info.id, tabId, message);               \
                browser.drain(runtimeSendMessageResult, success, {}, 5000); \
            });                                                         \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.sendMessage");
        BIND_FN(context, "tabs", "insertCSS", "tabs.insertCSS = function (one, two) {\
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
tabs.insertCSS");
        BIND_FN(context, "tabs", "removeCSS", "tabs.removeCSS = function (one, two) {\
    var tabId = (two === undefined) ? 0 : one;                          \
    var css = (two === undefined) ? one : two;                          \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            management.getSelf().then(function (info) {                 \
                tabsRemoveCSS(info.id, tabId, css);                     \
                setTimeout(() => success(),                             \
                           10);});                                      \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.removeCSS");
        BIND_FN(context, "tabs", "executeScript", "tabs.executeScript = function (one, two) {\
    var tabId = (two === undefined) ? 0 : one;                          \
    var script = (two === undefined) ? one : two;                       \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            if (script.allFrames && script.frameId)                     \
                throw new Error(\"frameId and allFrames are mutually exclusive.\"); \
            management.getSelf().then(function (info) {                 \
                tabsRemoveCSS(info.id, tabId, script);                  \
                setTimeout(() => success([]),                           \
                           0);});                                       \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.executeScript")
        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                "print", print);
        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                                      "TAB_ID_NONE", jsc_value_new_number(context, 0));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "tabs",
                jsc_context_evaluate(context, "tabs", -1));
}
