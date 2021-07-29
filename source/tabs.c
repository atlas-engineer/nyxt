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
                context, "tabsQuery",
                G_CALLBACK(tabs_query_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *print = jsc_value_new_function(
                context, NULL, G_CALLBACK(tabs_print_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCClass *Tabs = jsc_context_register_class(context, "Tabs", NULL, NULL, NULL);
        JSCValue *Tabs_constructor = jsc_class_add_constructor(
                Tabs, NULL, G_CALLBACK(empty_constructor_callback),
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCValue *tabs = jsc_value_new_object(context, NULL, Tabs);
        char *tabs_js = malloc(sizeof(char) * 900);
        jsc_context_set_value(context, "Tabs", Tabs_constructor);
        jsc_context_set_value(context, "tabsQuery", tabsQuery);
        jsc_context_set_value(context, "tabsQueryResult", tabsQueryResult);
        sprintf(tabs_js, "tabs.query = function (queryObject) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsQuery(queryObject);                                     \
            return function () {                                        \
                return success(tabsQueryResult(\"%s\"));                \
            }();                                                        \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.query", extension_name);
        jsc_value_object_set_property(
                tabs, "query", jsc_context_evaluate(context, tabs_js, -1));
        jsc_value_object_set_property(tabs, "print", print);
        free(tabs_js);
        jsc_context_set_value(context, "tabs", tabs);
}
