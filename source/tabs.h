#ifndef __TABS_H__
#define __TABS_H__

typedef struct {
        char *tabs;
        char *created_tab;
        char *current_tab;
} Tabs;

extern Tabs *TABS;

void inject_tabs_api (WebKitWebPage *web_page, char* extension_name);

#endif /* __TABS_H__ */
