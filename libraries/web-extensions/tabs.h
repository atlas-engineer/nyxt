#ifndef __TABS_H__
#define __TABS_H__

typedef struct {
        char *tabs;
        char *tab;
        char *reply;
} Tabs;

extern Tabs *TABS;

void inject_tabs_api (char* extension_name);

#endif /* __TABS_H__ */
