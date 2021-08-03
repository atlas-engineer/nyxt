#ifndef __RUNTIME_H__
#define __RUNTIME_H__

typedef struct {
        char *reply;
} Runtime;

extern Runtime *RUNTIME;

void inject_runtime_api (char* extension_name);

#endif /* __RUNTIME_H__ */
