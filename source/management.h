#ifndef __MANAGEMENT_H__
#define __MANAGEMENT_H__

typedef struct {
        char *info;
} Management;

extern Management *MANAGEMENT;

void inject_management_api (char* extension_name);

#endif /* __MANAGEMENT_H__ */
