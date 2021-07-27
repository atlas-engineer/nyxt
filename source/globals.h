#include <webkit2/webkit-web-extension.h>
#include <json-glib/json-glib.h>

/** PAGES stores all the existing WebKitWebPages, indexed by their IDs.
 */
extern WebKitWebPage *PAGE;

typedef struct {
        GRecMutex lock;
        char *tabs;
} Tabs;

extern Tabs *TABS;

typedef struct {
        JSCContext *context;
        char *name;
        char *permissions;
        WebKitScriptWorld *world;
} ExtensionData;

extern GHashTable *EXTENSIONS_DATA;

extern WebKitWebExtension *EXTENSION;

void extensions_data_add_from_json_root(JsonNode *root, WebKitWebPage *web_page);
