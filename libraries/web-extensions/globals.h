// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

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

/** ERROR_MESSAGE_PREFIX
 *
 * What string to consider starting the error serialization. Use this
 * string to start the broken result when returning the error from the
 * browser in response to extension message.
 *
 * Example: If the extension has no "tabs" permission and tries to
 * call tabs.query({url: "whatever"}), you can respond to this message
 * with "error: Extension has to 'tabs' access" if your
 * ERROR_MESSAGE_PREFIX is "error: ".
 */
#ifndef ERROR_MESSAGE_PREFIX
#define ERROR_MESSAGE_PREFIX "error: "
#endif

/** WINDOW_ID_NONE
 *
 * What (integer) number to use as browser.windows.WINDOW_ID_NONE constant.
 */
#ifndef WINDOW_ID_NONE
#define WINDOW_ID_NONE 0
#endif

/** TAB_ID_NONE
 *
 * What (integer) number to use as browser.tabs.TAB_ID_NONE constant.
 */
#ifndef TAB_ID_NONE
#define TAB_ID_NONE 0
#endif

/** WINDOW_ID_CURRENT
 *
 * What (integer) number to use as browser.windows.WINDOW_ID_CURRENT constant.
 */
#ifndef WINDOW_ID_CURRENT
#define WINDOW_ID_CURRENT -1
#endif

/** TODO_METHOD
 *
 * A macro to mark the methods that are not yet implemented. Outputs
 * "$METHOD is not yet implemented" to the shell.
 */
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

/** TODO_PROP
 *
 * A macro to mark the object properties that are not yet
 * implemented. Outputs "$PROPERTY is not yet implemented" to the
 * shell.
 *
 * FIXME: It doesn't. Broken.
 */
#define TODO_PROP(Class, Property)                                      \
        do {                                                            \
                jsc_class_add_property(                                 \
                        Class, #Property,                               \
                        JSC_TYPE_VALUE,                                 \
                        G_CALLBACK(todo_property_callback),             \
                        NULL, #Class "." #Property, NULL);              \
        } while (0);                                                    \

/* Small convenience macro to evaluate JS code of arbitrary length. */
#define JSCEVAL(Context, ...)                           \
        jsc_context_evaluate(Context, __VA_ARGS__, -1)

/** MAKE_CLASS
 *
 * Create a JSCClass and register and empty constructor for it. Bind
 * it to Object_name in Context.
 */
#define MAKE_CLASS(Context, Class, Object_name)                              \
        JSCClass *Class = jsc_context_register_class(                   \
                Context, #Class, NULL, NULL, NULL);                     \
        JSCValue *Class##_constructor = jsc_class_add_constructor(      \
                Class, NULL, G_CALLBACK(empty_constructor_callback),    \
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);               \
        jsc_context_set_value(Context, #Class, Class##_constructor);    \
        jsc_context_set_value(Context, Object_name,                     \
                jsc_value_new_object(Context, NULL, Class));

/** MAKE_FN
 *
 * Create a JS function and bind it to Prop_name (literal string) of
 * Object_name (literal string) in Context. Bind Callback to it, with
 * User_data passed as the last argument (if not NULL). Varargs are a
 * number of arguments and GTypes of those (see
 * jsc_value_new_function).
 */
#define MAKE_FN(Context, Object_name, Prop_name, Callback, User_data, ...) \
        jsc_value_object_set_property(                                  \
                JSCEVAL(Context, Object_name), Prop_name,               \
                jsc_value_new_function(                                 \
                        Context, NULL, G_CALLBACK(Callback),            \
                        (void *)User_data, NULL, __VA_ARGS__));         \

/** MAKE_FNV
 *
 * Create a variadic function. Bind it to Prop_name (literal string)
 * of Object_name (literal string) in Context. Callback and User_data
 * work like in MAKE_FN, except for the fact that Callback always
 * accepts GPtrArray of JSValues (see
 * jsc_value_new_function_variadic).
 */
#define MAKE_FNV(Context, Object_name, Prop_name, Callback, User_data)  \
        jsc_value_object_set_property(                                  \
                JSCEVAL(Context, Object_name), Prop_name,               \
                jsc_value_new_function_variadic(                        \
                        Context, NULL, G_CALLBACK(Callback),            \
                        (void *)User_data, NULL, JSC_TYPE_VALUE));      \

/** MAKE_EVENT
 *
 * Created an ExtEvent (JS class this library introduces for
 * WebExtensions events) and binds it to Prop_name (literal string) of
 * Object_name (literal string) in Context.
 *
 */
#define MAKE_EVENT(Context, Object_name, Prop_name)            \
        jsc_value_object_set_property(                              \
                JSCEVAL(Context, Object_name), Prop_name,           \
                jsc_value_constructor_call(                         \
                        jsc_context_get_value(context, "ExtEvent"), \
                        JSC_TYPE_VALUE, jsc_value_new_null(Context),    \
                        G_TYPE_NONE));                              \
/** MAKE_EVENT_FILTERED
 *
 * Creates an event, much line MAKE_EVENT does. The only difference is
 * the ability to pass custom run filter.
 *
 * Varargs should be a JSCValue pointer or a form returning it. It
 * will be used as a filter to decide whether to run the listener
 * given listener args and run args at the moment of event
 * invocation. Use undefined or null JSCValues if you want to accept
 * all listeners.
 */
#define MAKE_EVENT_FILTERED(Context, Object_name, Prop_name, ...)   \
        jsc_value_object_set_property(                              \
                JSCEVAL(Context, Object_name), Prop_name,           \
                jsc_value_constructor_call(                         \
                        jsc_context_get_value(context, "ExtEvent"), \
                        JSC_TYPE_VALUE, __VA_ARGS__,                \
                        G_TYPE_NONE));                              \

/** BIND_FN
 *
 * Bind a result of JS-evaluation of varargs to Prop_name (literal
 * string) of Object_name (literal string) in Context.
 *
 * Varargs are expected to be a string.
 */
#define BIND_FN(Context, Object_name, Prop_name, ...)               \
        do {                                                        \
                jsc_value_object_set_property(                      \
                        JSCEVAL(Context, Object_name), Prop_name,   \
                        JSCEVAL(Context, __VA_ARGS__));             \
        } while (0);

/** SEND_MESSAGE_RETURN_PROMISE
 *
 * Creates a promise waiting on the message callback and resolving
 * itself when there's a result. This is the macro that you want to
 * end most of your Promise-returning C callbacks with.
 */
#define SEND_MESSAGE_RETURN_PROMISE(Message, Context, Index_name)       \
        unsigned long int Index_name = get_next_data_counter();         \
        webkit_web_page_send_message_to_view(                           \
                PAGE, Message, NULL, message_reply_and_save_callback,   \
                (void*) Index_name);                                    \
        return make_promise(Context, Index_name);                       \

extern WebKitWebPage *PAGE;

typedef struct {
        char *name;
        char *extension_id;
        char *tab_id;
        int is_injected;
        char *manifest;
        int is_privileged;
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

int match_pattern_match (char *match_pattern, char *uri);

int has_permission (char* extension_name, char* permission);

void *empty_constructor_callback (void);

void message_reply_and_save_callback (GObject *web_page, GAsyncResult *res, void *user_data);

JSCValue *get_result (unsigned long int data_index, int check_only);

JSCValue *todo_method_callback(GPtrArray *args, void *user_data);

JSCValue *todo_property_callback(void *instance, void *user_data);

JSCValue *make_promise(JSCContext *context, unsigned long int id);

#endif /* __GLOBALS_H__ */
