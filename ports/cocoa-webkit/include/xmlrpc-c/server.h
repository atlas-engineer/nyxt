/* Copyright and license information is at the end of the file */

#ifndef  XMLRPC_SERVER_H_INCLUDED
#define  XMLRPC_SERVER_H_INCLUDED

#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/base.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
  XMLRPC_SERVER_EXPORTED marks a symbol in this file that is exported from
  libxmlrpc_server.

  XMLRPC_BUILDING_SERVER says this compilation is part of libxmlrpc_server, as
  opposed to something that _uses_ libxmlrpc_server.
*/
#ifdef XMLRPC_BUILDING_SERVER
#define XMLRPC_SERVER_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_SERVER_EXPORTED
#endif

typedef struct xmlrpc_registry xmlrpc_registry;

typedef void
(*xmlrpc_preinvoke_method)(xmlrpc_env *   const envP,
                           const char *   const methodName,
                           xmlrpc_value * const paramArrayP,
                           void *         const userData);

typedef xmlrpc_value *
(*xmlrpc_method1)(xmlrpc_env *   const envP,
                  xmlrpc_value * const paramArrayP,
                  void *         const serverInfo);

typedef xmlrpc_value *
(*xmlrpc_method2)(xmlrpc_env *   const envP,
                  xmlrpc_value * const paramArrayP,
                  void *         const serverInfo,
                  void *         const callInfo);

typedef xmlrpc_method1 xmlrpc_method;  /* backward compatibility */

typedef xmlrpc_value *
(*xmlrpc_default_method)(xmlrpc_env *   const envP,
                         const char *   const callInfoP,
                         const char *   const methodName,
                         xmlrpc_value * const paramArrayP,
                         void *         const serverInfo);

/* These are for backward compatibility -- they can't be exported from a
   Windows DLL.  xmlrpc_server_version() is preferred.
*/
extern unsigned int const xmlrpc_server_version_major;
extern unsigned int const xmlrpc_server_version_minor;
extern unsigned int const xmlrpc_server_version_point;

XMLRPC_SERVER_EXPORTED
void
xmlrpc_server_version(unsigned int * const majorP,
                      unsigned int * const minorP,
                      unsigned int * const pointP);

XMLRPC_SERVER_EXPORTED
xmlrpc_registry *
xmlrpc_registry_new(xmlrpc_env * const envP);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_free(xmlrpc_registry * const registryP);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_disable_introspection(xmlrpc_registry * const registryP);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_add_method(xmlrpc_env *      const envP,
                           xmlrpc_registry * const registryP,
                           const char *      const host,
                           const char *      const methodName,
                           xmlrpc_method     const method,
                           void *            const serverInfo);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_add_method_w_doc(xmlrpc_env *      const envP,
                                 xmlrpc_registry * const registryP,
                                 const char *      const host,
                                 const char *      const methodName,
                                 xmlrpc_method     const method,
                                 void *            const serverInfo,
                                 const char *      const signatureString,
                                 const char *      const help);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_add_method2(xmlrpc_env *      const envP,
                            xmlrpc_registry * const registryP,
                            const char *      const methodName,
                            xmlrpc_method2          method,
                            const char *      const signatureString,
                            const char *      const help,
                            void *            const serverInfo);

struct xmlrpc_method_info3 {
    const char *      methodName;
    xmlrpc_method2    methodFunction;
    void *            serverInfo;
    size_t            stackSize;
    const char *      signatureString;
    const char *      help;
};

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_add_method3(
    xmlrpc_env *                       const envP,
    xmlrpc_registry *                  const registryP,
    const struct xmlrpc_method_info3 * const infoP);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_set_default_method(xmlrpc_env *          const envP,
                                   xmlrpc_registry *     const registryP,
                                   xmlrpc_default_method const handler,
                                   void *                const userData);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_set_preinvoke_method(xmlrpc_env *            const envP,
                                     xmlrpc_registry *       const registryP,
                                     xmlrpc_preinvoke_method const method,
                                     void *                  const userData);


typedef void xmlrpc_server_shutdown_fn(xmlrpc_env * const envP,
                                       void *       const context,
                                       const char * const comment,
                                       void *       const callInfo);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_set_shutdown(xmlrpc_registry *           const registryP,
                             xmlrpc_server_shutdown_fn * const shutdownFn,
                             void *                      const context);

XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_set_dialect(xmlrpc_env *      const envP,
                            xmlrpc_registry * const registryP,
                            xmlrpc_dialect    const dialect);

/*----------------------------------------------------------------------------
   Lower interface -- services to be used by an HTTP request handler
-----------------------------------------------------------------------------*/
                    
XMLRPC_SERVER_EXPORTED
void
xmlrpc_registry_process_call2(xmlrpc_env *        const envP,
                              xmlrpc_registry *   const registryP,
                              const char *        const xmlData,
                              size_t              const xmlLen,
                              void *              const callInfo,
                              xmlrpc_mem_block ** const outputPP);

XMLRPC_SERVER_EXPORTED
xmlrpc_mem_block *
xmlrpc_registry_process_call(xmlrpc_env *      const envP,
                             xmlrpc_registry * const registryP,
                             const char *      const host,
                             const char *      const xmlData,
                             size_t            const xmlLen);

XMLRPC_SERVER_EXPORTED
size_t
xmlrpc_registry_max_stackSize(xmlrpc_registry * const registryP);

#ifdef __cplusplus
}
#endif

/* Copyright (C) 2001 by First Peer, Inc. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
** 3. The name of the author may not be used to endorse or promote products
**    derived from this software without specific prior written permission. 
**  
** THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
** SUCH DAMAGE. */
#endif
