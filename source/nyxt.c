#include <webkit2/webkit-web-extension.h>

static WebKitScriptWorld *hello_world = NULL;

static gboolean
user_message_received (WebKitWebPage     *web_page,
                       WebKitUserMessage *message,
                       gpointer           user_data)
{
        WebKitFrame *frame = webkit_web_page_get_main_frame(web_page);
        if (hello_world == NULL)
                hello_world = webkit_script_world_new_with_name("hello");
        JSCContext *hello_context = webkit_frame_get_js_context_for_script_world(frame, hello_world);
        int hello = jsc_value_to_int32(jsc_context_evaluate(hello_context, "foo()", -1));
        g_print ("hello is \"%d\"\n", hello);
        return FALSE;
}

static int foo(void)
{
    return 2;
}

static void
document_loaded_callback (WebKitWebPage *web_page)
{
        WebKitFrame *frame = webkit_web_page_get_main_frame(web_page);
        if (hello_world == NULL)
                hello_world = webkit_script_world_new_with_name("hello");
        JSCContext *hello_context = webkit_frame_get_js_context_for_script_world(frame, hello_world);
        JSCValue *hello_fn;
        if (jsc_value_is_undefined(jsc_context_evaluate(hello_context, "foo", -1))) {
                hello_fn = jsc_value_new_function(
                        hello_context, "foo", G_CALLBACK(foo), NULL, NULL,
                        G_TYPE_INT, 0, G_TYPE_NONE);
                jsc_context_set_value(hello_context, "foo", hello_fn);
                g_print ("Defined foo\n");
        } else {
                hello_fn = jsc_context_evaluate(hello_context, "foo", -1);
        }
        int hello = jsc_value_to_int32(jsc_value_function_call(hello_fn, G_TYPE_NONE));
        g_print ("Page %lu created for \"%s\", hello is \"%d\"\n",
                 webkit_web_page_get_id (web_page),
                 webkit_web_page_get_uri (web_page),
                 hello);
}

static void
web_page_created_callback (WebKitWebExtension *extension,
                           WebKitWebPage      *web_page,
                           gpointer            user_data)
{
        g_print ("Page %lu created for \"%s\"\n",
                 webkit_web_page_get_id (web_page),
                 webkit_web_page_get_uri (web_page));
        g_signal_connect (web_page, "document-loaded",
                          G_CALLBACK (document_loaded_callback),
                          NULL);
        g_signal_connect (web_page, "user-message-received",
                          G_CALLBACK (user_message_received),
                          NULL);
}

G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
}
