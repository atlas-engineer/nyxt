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
tabs_query_result_callback (char *extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        return jsc_value_new_string(context, TABS->tabs);
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
tabs_print_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("print", NULL);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
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
        TABS->created_tab = (char *) json;
}

static JSCValue *
tabs_create_result_callback (char *extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        return jsc_value_new_string(context, TABS->created_tab);
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
        TABS->current_tab = (char *) json;
}

static JSCValue *
tabs_get_current_result_callback (char *extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        return jsc_value_new_string(context, TABS->current_tab);
}

static void
tabs_get_current_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.getCurrent", NULL);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_get_current_reply_callback, NULL);
}

void
inject_tabs_api (WebKitWebPage *web_page, char* extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(web_page);
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        JSCValue *tabsQuery = jsc_value_new_function(
                context, "tabsQuery",
                G_CALLBACK(tabs_query_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        JSCValue *tabsQueryResult = jsc_value_new_function(
                context, "tabsQueryResult",
                G_CALLBACK(tabs_query_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *tabsCreate = jsc_value_new_function(
                context, "tabsCreate",
                G_CALLBACK(tabs_create_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        JSCValue *tabsCreateResult = jsc_value_new_function(
                context, "tabsCreateResult",
                G_CALLBACK(tabs_create_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *tabsGetCurrent = jsc_value_new_function(
                context, "tabsGetCurrent",
                G_CALLBACK(tabs_get_current_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCValue *tabsGetCurrentResult = jsc_value_new_function(
                context, "tabsGetCurrentResult",
                G_CALLBACK(tabs_get_current_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *print = jsc_value_new_function(
                context, NULL, G_CALLBACK(tabs_print_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCClass *Tabs = jsc_context_register_class(context, "Tabs", NULL, NULL, NULL);
        JSCValue *Tabs_constructor = jsc_class_add_constructor(
                Tabs, NULL, G_CALLBACK(empty_constructor_callback),
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);
        char *tabs_query_js = malloc(sizeof(char) * 900),
                *tabs_create_js = malloc(sizeof(char) * 900),
                *tabs_get_current_js = malloc(sizeof(char) * 900);
        jsc_context_set_value(context, "Tabs", Tabs_constructor);
        jsc_context_set_value(context, "tabsQuery", tabsQuery);
        jsc_context_set_value(context, "tabsQueryResult", tabsQueryResult);
        jsc_context_set_value(context, "tabsCreate", tabsCreate);
        jsc_context_set_value(context, "tabsCreateResult", tabsCreateResult);
        jsc_context_set_value(context, "tabsGetCurrent", tabsGetCurrent);
        jsc_context_set_value(context, "tabsGetCurrentResult", tabsGetCurrentResult);
        jsc_context_set_value(context, "tabs", jsc_value_new_object(context, NULL, Tabs));
        sprintf(tabs_query_js, "tabs.query = function (queryObject) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsQuery(queryObject);                                     \
            setTimeout(() => success(tabsQueryResult(\"%s\")), 20);     \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.query", extension_name);
        sprintf(tabs_create_js, "tabs.create = function (createProperties) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsCreate(createProperties);                                     \
            setTimeout(() => success(tabsCreateResult(\"%s\")), 20);     \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.create", extension_name);
        sprintf(tabs_get_current_js, "tabs.getCurrent = function () { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsGetCurrent();                                           \
            setTimeout(() => success(tabsGetCurrentResult(\"%s\")), 20);     \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.getCurrent", extension_name);
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
        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                                      "print", print);
        free(tabs_query_js);
        free(tabs_create_js);
        free(tabs_get_current_js);
}
