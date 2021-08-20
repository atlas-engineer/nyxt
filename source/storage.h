#ifndef __STORAGE_H__
#define __STORAGE_H__

typedef struct {
        char *data;
} Storage;

extern Storage *STORAGE;

void inject_storage_api (char* extension_name);

#endif /* __STORAGE_H__ */
