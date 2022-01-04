// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#ifndef __RUNTIME_H__
#define __RUNTIME_H__

char * runtime_get_url_callback (char *path, void* extension_name);

void inject_runtime_api (char* extension_name);

#endif /* __RUNTIME_H__ */
