#include "globals.h"

WebKitWebPage *PAGE;

Tabs *TABS;

GHashTable *EXTENSIONS_DATA;

WebKitWebExtension *EXTENSION;

void
extensions_data_add_from_json_root(JsonNode *root, WebKitWebPage *web_page)
{
        JsonObject *obj = json_node_get_object(root);
        GList *members = json_object_get_members(obj);
        WebKitFrame *frame = webkit_web_page_get_main_frame(web_page);
        for (;members != NULL; members = members->next){
                ExtensionData *extension;
                const char *name = members->data,
                        *permissions = json_node_get_string(
                                json_object_get_member(obj, name));
                g_print("Name is \"%s\", permissions is \"%s\"\n", name, permissions);
                g_print("EXTENSIONS_DATA is %p\n", EXTENSIONS_DATA);
                g_print("Extension will be allocated\n");
                extension = malloc(sizeof(ExtensionData));
                g_print("Extension allocated\n");
                extension->name = (char*) name;
                g_print("Extension name set\n");
                extension->permissions = (char*) permissions;
                g_print("Extension permissions set\n");
                extension->world = webkit_script_world_new_with_name(name);
                g_print("Extension world created\n");
                extension->context = webkit_frame_get_js_context_for_script_world(
                        frame, extension->world);
                g_print("Extension contexts created\n");
                g_hash_table_insert(EXTENSIONS_DATA, (void*) name, extension);
                g_print("Extension created\n");
        }
}
