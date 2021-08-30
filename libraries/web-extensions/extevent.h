#ifndef __EXTEVENT_H__
#define __EXTEVENT_H__

typedef struct {
        GPtrArray* listeners;
} Extevent;

void inject_extevent_api (char* extension_name);

#endif /* __EXTEVENT_H__ */
