#ifndef __TABS_H__
#define __TABS_H__

typedef struct {
        GThread *return_thread;
        char *tabs;
} Tabs;

extern Tabs *TABS;

void inject_tabs_api (WebKitWebPage *web_page, char* extension_name);

#endif /* __TABS_H__ */
