#include "globals.h"
#include "tabs.h"

Tabs *TABS;

static void*
return_tabs_thread (void *json)
{
        g_print("Return tabs thread entered\n");
        TABS->tabs = (char *) json;
        return TABS->tabs;
}

static void
tabs_query_reply_callback (GObject *web_page,
                           GAsyncResult *res,
                           gpointer user_data)
{
        g_print("tabs_query_reply_callback: Thread is %p\n", g_thread_self());
        g_print("tabsQuery reply callback entered\n");
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        g_print("Message finished\n");
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        g_print("Params are \"%s\"", json);
        if (!json)
                json = "[]";
        TABS->return_thread = g_thread_new(NULL, return_tabs_thread, json);
        g_print("Tabs-returning thread created\n");
}

static void
wait_result_thread (GTask         *task,
                    void*       source_object,
                    void*       message,
                    GCancellable  *cancellable)
{
        g_print("wait_result_thread: Thread is %p\n", g_thread_self());
        g_print("Waiting for tabs\n");
        while (!TABS->tabs)
                g_thread_yield();
        g_task_return_pointer(task, TABS->tabs, free);
}

static JSCValue *
tabs_query_result_callback (char *extension_name)
{
        g_print("tabs_query_callback: Thread is %p\n", g_thread_self());
        g_print("tabsQueryResult callback entered\n");
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        g_print("Data acquired\n");
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        g_print("Main frame is there\n");
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        g_print("Context is there\n");
        g_print("TABS->tabs are: \"%s\"\n", TABS->tabs);
        return jsc_value_new_string(context, TABS->tabs);
}

static void
tabs_query_callback (JSCValue *object)
{
        g_print("tabs_query_callback: Thread is %p\n", g_thread_self());
        g_print("tabsQuery callback entered\n");
        char *json = jsc_value_to_json(object, 0);
        g_print("JSON of queryObject is %s\n", json);
        GVariant *variant = g_variant_new("s", json);
        g_print("GVariant created\n");
        WebKitUserMessage *message = webkit_user_message_new("tabs.queryObject", variant);
        g_print("Message created\n");
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_query_reply_callback, NULL);
}

void
inject_tabs_api (WebKitWebPage *web_page, char* extension_name)
{
        g_print("Extension name is %s\n", extension_name);
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(web_page);
        g_print("World name is %s\n", webkit_script_world_get_name(data->world));
        JSCContext *context = webkit_frame_get_js_context_for_script_world(frame, data->world);
        g_print("Contexts are: %p\n", context);
        JSCValue *tabsQuery = jsc_value_new_function(
                context, "tabsQuery",
                G_CALLBACK(tabs_query_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        g_print("tabsQuery created\n");
        JSCValue *tabsQueryResult = jsc_value_new_function(
                context, "tabsQuery",
                G_CALLBACK(tabs_query_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        g_print("tabsQueryResult created\n");
        JSCClass *Tabs = jsc_context_register_class(context, "Tabs", NULL, NULL, NULL);
        g_print("Tabs registered\n");
        JSCValue *Tabs_constructor = jsc_class_add_constructor(
                Tabs, NULL, G_CALLBACK(empty_constructor_callback),
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);
        g_print("Tabs constructor created\n");
        JSCValue *tmp;
        char *tabs_js = malloc(sizeof(char) * 900);
        jsc_context_set_value(context, "Tabs", Tabs_constructor);
        g_print("Tabs constructor set\n");
        jsc_context_set_value(context, "tabsQuery", tabsQuery);
        g_print("tabsQuery set\n");
        jsc_context_set_value(context, "tabsQueryResult", tabsQueryResult);
        g_print("tabsQueryResult set\n");
        jsc_context_set_value(context, "tabs", jsc_value_new_object(context, NULL, Tabs));
        sprintf(tabs_js, "tabs.query = function (queryObject) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsQuery(queryObject);                                     \
            setTimeout(() => success(tabsQueryResult(\"%s\")), 70);     \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.query", extension_name);
        g_print("Tabs JS created\n");
        tmp = jsc_context_evaluate(context, tabs_js, -1);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1), "query", tmp);
        g_print("Tabs JS evaluated\n");
        free(tabs_js);
}
