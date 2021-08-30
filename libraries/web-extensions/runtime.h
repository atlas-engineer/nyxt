#ifndef __RUNTIME_H__
#define __RUNTIME_H__

typedef struct {
        char *reply;
        char *platform_info;
        char *browser_info;
} Runtime;

extern Runtime *RUNTIME;

void inject_runtime_api (char* extension_name);

#endif /* __RUNTIME_H__ */
