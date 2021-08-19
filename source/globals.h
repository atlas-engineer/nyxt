#ifndef __GLOBALS_H__
#define __GLOBALS_H__

#include <webkit2/webkit-web-extension.h>

#define JSCEVAL(Context, ...)                           \
        jsc_context_evaluate(Context, __VA_ARGS__, -1)

#define MAKE_CLASS(Context, Class, Object_name)                         \
        JSCClass *Class = jsc_context_register_class(                   \
                Context, #Class, NULL, NULL, NULL);                     \
        JSCValue *Class##_constructor = jsc_class_add_constructor(      \
                Class, NULL, G_CALLBACK(empty_constructor_callback),    \
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);               \
        jsc_context_set_value(Context, #Class, Class##_constructor);    \
        jsc_context_set_value(                                          \
                Context, Object_name,                                   \
                jsc_value_new_object(context, NULL, Class));            \

#define MAKE_FN(Context, Fn, Callback, ...)                             \
        JSCValue *Fn = jsc_value_new_function(                          \
                Context, #Fn, G_CALLBACK(Callback), NULL, NULL,         \
                __VA_ARGS__);                                           \
        jsc_context_set_value(Context, #Fn, Fn);                        \

#define MAKE_RESULT_FN(Context, Fn, User_data)                          \
        JSCValue *Fn = jsc_value_new_function(                          \
                Context, #Fn, G_CALLBACK(get_result_callback),          \
                User_data, NULL,                                        \
                JSC_TYPE_VALUE, 0, G_TYPE_NONE);                        \
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

extern WebKitWebPage *PAGE;

typedef struct {
        char *name;
        JSCValue *manifest;
        WebKitScriptWorld *world;
} ExtensionData;

extern GHashTable *EXTENSIONS_DATA;

extern GHashTable *MESSAGES;

extern WebKitWebExtension *EXTENSION;

void extensions_data_add_from_json(const char *json);

WebKitScriptWorld *get_extension_world (char* extension_name);

JSCContext *get_extension_context (char* extension_name);

void *empty_constructor_callback (void);

void message_reply_and_save_callback (GObject *web_page, GAsyncResult *res, void *user_data);

JSCValue *get_result_callback (void *user_data);

#endif /* __GLOBALS_H__ */
