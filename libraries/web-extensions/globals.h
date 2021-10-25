#ifndef __GLOBALS_H__
#define __GLOBALS_H__

#include <webkit2/webkit-web-extension.h>

/** PRIVILEGED_SCHEME
 *
 * The scheme to allow browser.eval() on.
 */
#ifndef PRIVILEGED_SCHEME
#define PRIVILEGED_SCHEME "nyxt:"
#endif

/** BROWSER_REPLY_TIMEOUT
 *
 * For how much milliseconds to wait for the browser replying to the
 * API message. */
#ifndef BROWSER_REPLY_TIMEOUT
#define BROWSER_REPLY_TIMEOUT 5000
#endif

#define TODO_METHOD(Context, Object, Method)                            \
        do {                                                            \
                jsc_value_object_set_property(                          \
                        JSCEVAL(Context, #Object), #Method,             \
                        jsc_value_new_function_variadic(                \
                                Context, NULL,                          \
                                G_CALLBACK(todo_method_callback),       \
                                #Object "." #Method,                    \
                                NULL, JSC_TYPE_VALUE));                 \
        } while (0);

#define TODO_PROP(Class, Property)                                      \
        do {                                                            \
                jsc_class_add_property(                                 \
                        Class, #Property,                               \
                        JSC_TYPE_VALUE,                                 \
                        G_CALLBACK(todo_property_callback),             \
                        NULL, #Class "." #Property, NULL);              \
        } while (0);                                                    \

#define JSCEVAL(Context, ...)                           \
        jsc_context_evaluate(Context, __VA_ARGS__, -1)

#define MAKE_CLASS(Context, Class, Object_name)                              \
        JSCClass *Class = jsc_context_register_class(                   \
                Context, #Class, NULL, NULL, NULL);                     \
        JSCValue *Class##_constructor = jsc_class_add_constructor(      \
                Class, NULL, G_CALLBACK(empty_constructor_callback),    \
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);               \
        jsc_context_set_value(Context, #Class, Class##_constructor);    \
        jsc_context_set_value(Context, Object_name,                     \
                jsc_value_new_object(Context, NULL, Class));

#define MAKE_FN(Context, Fn, Callback, ...)                             \
        JSCValue *Fn = jsc_value_new_function(                          \
                Context, #Fn, G_CALLBACK(Callback), NULL, NULL,         \
                __VA_ARGS__);                                           \
        jsc_context_set_value(Context, #Fn, Fn);                        \

#define MAKE_EVENT(Context, Object_name, Prop_name)         \
        jsc_value_object_set_property(                      \
                JSCEVAL(Context, Object_name), Prop_name,   \
                JSCEVAL(Context, "new ExtEvent()"));        \

#define BIND_FN(Context, Object_name, Prop_name, ...)               \
        do {                                                        \
                jsc_value_object_set_property(                      \
                        JSCEVAL(Context, Object_name), Prop_name,   \
                        JSCEVAL(Context, __VA_ARGS__));             \
        } while (0);

#define SEND_MESSAGE_RETURN_ID(Message, Index_name)                     \
        unsigned long int Index_name = get_next_data_counter();      \
        webkit_web_page_send_message_to_view(                           \
                PAGE, Message, NULL, message_reply_and_save_callback,   \
                (void*) Index_name);                                    \
        return Index_name;                                              \

extern WebKitWebPage *PAGE;

typedef struct {
        char *name;
        char *extension_id;
        char *manifest;
        JSCValue *files;
        WebKitScriptWorld *world;
} ExtensionData;

extern GHashTable *EXTENSIONS_DATA;

extern WebKitWebExtension *EXTENSION;

extern int IS_PRIVILEGED;

extern unsigned long int DATA_COUNTER;

extern GHashTable *DATA;

unsigned long int get_next_data_counter ();

void extensions_data_add_from_json (const char *json);

WebKitScriptWorld *get_extension_world (char* extension_name);

JSCContext *get_extension_context (char* extension_name);

char *get_extension_id (char* extension_name);

int has_permission (char* extension_name, char* permission);

void *empty_constructor_callback (void);

void message_reply_and_save_callback (GObject *web_page, GAsyncResult *res, void *user_data);

JSCValue *get_result (unsigned long int data_index, int check_only);

JSCValue *todo_method_callback(GPtrArray *args, void *user_data);

JSCValue *todo_property_callback(void *instance, void *user_data);

#endif /* __GLOBALS_H__ */
