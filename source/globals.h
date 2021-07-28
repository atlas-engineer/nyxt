#ifndef __GLOBALS_H__
#define __GLOBALS_H__

#include <webkit2/webkit-web-extension.h>
#include <json-glib/json-glib.h>

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

void *empty_constructor_callback (void);

#endif /* __GLOBALS_H__ */
