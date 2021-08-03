#ifndef __GLOBALS_H__
#define __GLOBALS_H__

#include <webkit2/webkit-web-extension.h>
#include <json-glib/json-glib.h>

#define MAKE_CLASS(Context, Class, Object_name)                         \
        JSCClass *Class = jsc_context_register_class(                   \
                Context, #Class, NULL, NULL, NULL);                     \
        JSCValue *Class##_constructor = jsc_class_add_constructor(      \
                Class, NULL, G_CALLBACK(empty_constructor_callback),    \
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);               \
        jsc_context_set_value(context, #Class, Class##_constructor);    \
        jsc_context_set_value(context, Object_name,                     \
                      jsc_value_new_object(context, NULL, Class));

/** PAGES stores all the existing WebKitWebPages, indexed by their IDs.
 */
extern WebKitWebPage *PAGE;

typedef struct {
        char *name;
        char *permissions;
        WebKitScriptWorld *world;
} ExtensionData;

extern GHashTable *EXTENSIONS_DATA;

extern WebKitWebExtension *EXTENSION;

void extensions_data_add_from_json_root(JsonNode *root, WebKitWebPage *web_page);

WebKitScriptWorld *get_extension_world (char* extension_name);

JSCContext *get_extension_context (char* extension_name);

void *empty_constructor_callback (void);

#endif /* __GLOBALS_H__ */
