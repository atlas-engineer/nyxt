/*============================================================================
                         xmlrpc_client.h
==============================================================================
  This header file defines the interface between xmlrpc.c and its users,
  related to clients.

  Copyright information is at the end of the file.
============================================================================*/

#ifndef  XMLRPC_CLIENT_H_INCLUDED
#define  XMLRPC_CLIENT_H_INCLUDED

#include <stdarg.h>
#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/base.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct xmlrpc_client;
struct xmlrpc_client_transport;
struct xmlrpc_client_transport_ops;
#ifndef __cplusplus
typedef struct xmlrpc_client xmlrpc_client;
typedef struct xmlrpc_client_transport xmlrpc_client_transport;
typedef struct xmlrpc_client_transport_ops xmlrpc_client_transport_ops;
#endif

/*
  XMLRPC_CLIENT_EXPORTED marks a symbol in this file that is exported from
  libxmlrpc_client.

  XMLRPC_BUILDING_CLIENT says this compilation is part of libxmlrpc_client, as
  opposed to something that _uses_ libxmlrpc_client.
*/
#ifdef XMLRPC_BUILDING_CLIENT
#define XMLRPC_CLIENT_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_CLIENT_EXPORTED
#endif

/* libxmlrpc_client typically does _not_ actually include all of the
   XML transports declared here by xmlrpc_*_transport_ops.

   Use 'xmlrpc-c-config --features' to determine which features are
   installed.
*/

/* Before Xmlrpc-c 1.13 (December 2007), we declared struct
   xmlrpc_xportparms, as a sort of "base class."  The struct was never
   complete -- you just cast pointer to it it to pointers to other
   types.  It turned out not to be really helpful and casts are ugly,
   so now we just use void * as a base class pointer.
*/

XMLRPC_CLIENT_EXPORTED
extern struct xmlrpc_client_transport_ops xmlrpc_libwww_transport_ops;
XMLRPC_CLIENT_EXPORTED
extern struct xmlrpc_client_transport_ops xmlrpc_wininet_transport_ops;
XMLRPC_CLIENT_EXPORTED
extern struct xmlrpc_client_transport_ops xmlrpc_curl_transport_ops;

enum xmlrpc_sslversion {
    XMLRPC_SSLVERSION_DEFAULT,
    XMLRPC_SSLVERSION_TLSv1,
    XMLRPC_SSLVERSION_SSLv2,
    XMLRPC_SSLVERSION_SSLv3
};


enum xmlrpc_httpauthtype {
    /* These are just constants.  They can be or'ed as integers to create
       a set.
    */
    XMLRPC_HTTPAUTH_BASIC         = (1<<0),
    XMLRPC_HTTPAUTH_DIGEST        = (1<<1),
    XMLRPC_HTTPAUTH_GSSNEGOTIATE  = (1<<2),
    XMLRPC_HTTPAUTH_NTLM          = (1<<3)
};

/* The following are useful combinations of the HTTP authentication types
   above.
*/
#define XMLRPC_HTTPAUTH_NONE 0
#define XMLRPC_HTTPAUTH_ANY  ~0
#define XMLRPC_HTTPAUTH_ANYSAFE (~XMLRPC_HTTPAUTH_BASIC)

enum xmlrpc_httpproxytype {
    XMLRPC_HTTPPROXY_HTTP   = 0,
    XMLRPC_HTTPPROXY_SOCKS5 = 5
};

struct xmlrpc_curl_xportparms {
    /* This is designed so that zero values are always the defaults. */
    const char * network_interface;
    xmlrpc_bool  no_ssl_verifypeer;
    xmlrpc_bool  no_ssl_verifyhost;
    const char * user_agent;
    const char * ssl_cert;
    const char * sslcerttype;
    const char * sslcertpasswd;
    const char * sslkey;
    const char * sslkeytype;
    const char * sslkeypasswd;
    const char * sslengine;
    xmlrpc_bool  sslengine_default;
    enum xmlrpc_sslversion sslversion;
    const char * cainfo;
    const char * capath;
    const char * randomfile;
    const char * egdsocket;
    const char * ssl_cipher_list;
    unsigned int timeout;
    xmlrpc_bool dont_advertise;
    const char * proxy;
    unsigned int proxy_port;
    enum xmlrpc_httpproxytype proxy_type;
    unsigned int proxy_auth;
        /* A set of authentication schemes -- an OR of
           enum xmlrpc_httpproxyauth values
        */
    const char * proxy_userpwd;
    xmlrpc_bool  gssapi_delegation;
    const char * referer;
};


#define XMLRPC_CXPSIZE(mbrname) \
    XMLRPC_STRUCTSIZE(struct xmlrpc_curl_xportparms, mbrname)

/* XMLRPC_CXPSIZE(xyz) is analogous to XMLRPC_CPSIZE, below */

struct xmlrpc_wininet_xportparms {
    int allowInvalidSSLCerts;
};

#define XMLRPC_WXPSIZE(mbrname) \
    XMLRPC_STRUCTSIZE(struct xmlrpc_wininet_xportparms, mbrname)

/* XMLRPC_WXPSIZE(xyz) is analogous to XMLRPC_CPSIZE, below */

struct xmlrpc_transfer_progress {
    double total;
    double now;
};

struct xmlrpc_progress_data {
    struct xmlrpc_transfer_progress call;
    struct xmlrpc_transfer_progress response;
};

typedef void xmlrpc_progress_fn(void * const,
                                struct xmlrpc_progress_data const);

struct xmlrpc_clientparms {
    /* (transport, transportparmsP, transportparm_size) and
       (transportOpsP, transportP) are mutually exclusive.
    */
    const char *               transport;
    const void *               transportparmsP;
        /* This should be type "const struct ..._xportparms *" */
    size_t                     transportparm_size;

    const struct xmlrpc_client_transport_ops * transportOpsP;
    xmlrpc_client_transport *  transportP;
    xmlrpc_dialect             dialect;
    xmlrpc_progress_fn *       progressFn;
};

#define XMLRPC_CPSIZE(mbrname) \
  XMLRPC_STRUCTSIZE(struct xmlrpc_clientparms, mbrname)

/* XMLRPC_CPSIZE(xyz) is the minimum size a struct xmlrpc_clientparms
   must be to include the 'xyz' member.  This is essential to forward and
   backward compatibility, as new members will be added to the end of the
   struct in future releases.  This is how the callee knows whether or
   not the caller is new enough to have supplied a certain parameter.
*/

XMLRPC_CLIENT_EXPORTED
const char * 
xmlrpc_client_get_default_transport(xmlrpc_env * const env);

/* A user's function to handle the response to an asynchronous call.
** If 'fault->fault_occurred' is true, then response will be NULL. All
** arguments except 'userHandle' will be deallocated internally; please do
** not free any of them yourself.
** WARNING: 'paramArray' may (or may not) be NULL if fault->fault_occurred
** is true, and you set up the call using xmlrpc_client_call_asynch.
** WARNING: If asynchronous calls are still pending when the library is
** shut down, your handler may (or may not) be called with a fault. */
typedef void xmlrpc_response_handler(const char *   serverUrl,
                                     const char *   methodName,
                                     xmlrpc_value * paramArray,
                                     void *         userHandle,
                                     xmlrpc_env *   fault,
                                     xmlrpc_value * result);


/*=========================================================================
   xmlrpc_server_info
===========================================================================
  We normally refer to servers by URL. But sometimes we need to do extra
  setup for particular servers. In that case, we can create an
  xmlrpc_server_info object, configure it in various ways, and call the
  remote server.

  (This interface is also designed to discourage further multiplication
  of xmlrpc_client_call APIs. We have enough of those already. Please
  add future options and flags using xmlrpc_server_info.)
=========================================================================*/

typedef struct _xmlrpc_server_info xmlrpc_server_info;

/* Create a new server info record, pointing to the specified server. */
XMLRPC_CLIENT_EXPORTED
xmlrpc_server_info *
xmlrpc_server_info_new(xmlrpc_env * const envP,
                       const char * const serverUrl);

/* Create a new server info record, with a copy of the old server. */
XMLRPC_CLIENT_EXPORTED
extern xmlrpc_server_info * 
xmlrpc_server_info_copy(xmlrpc_env *         const envP,
                        xmlrpc_server_info * const srcP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_free(xmlrpc_server_info * const serverP);


XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_server_info_set_user(xmlrpc_env *         const envP,
                            xmlrpc_server_info * const serverInfoP,
                            const char *         const username,
                            const char *         const password);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_server_info_set_basic_auth(xmlrpc_env *         const envP,
                                  xmlrpc_server_info * const serverP,
                                  const char *         const username,
                                  const char *         const password);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_allow_auth_basic(xmlrpc_env *         const envP,
                                    xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_disallow_auth_basic(xmlrpc_env *         const envP,
                                       xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_allow_auth_digest(xmlrpc_env *         const envP,
                                     xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_disallow_auth_digest(xmlrpc_env *         const envP,
                                        xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_allow_auth_negotiate(xmlrpc_env *         const envP,
                                        xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_disallow_auth_negotiate(xmlrpc_env *         const envP,
                                           xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_allow_auth_ntlm(xmlrpc_env *         const envP,
                                   xmlrpc_server_info * const sP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_server_info_disallow_auth_ntlm(xmlrpc_env *         const envP,
                                      xmlrpc_server_info * const sP);

/* These are for backward compatibility -- they can't be exported from a
   Windows DLL.  xmlrpc_server_version() is preferred.
*/
extern unsigned int const xmlrpc_client_version_major;
extern unsigned int const xmlrpc_client_version_minor;
extern unsigned int const xmlrpc_client_version_point;

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_version(unsigned int * const majorP,
                      unsigned int * const minorP,
                      unsigned int * const pointP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_setup_global_const(xmlrpc_env * const envP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_teardown_global_const(void);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_create(xmlrpc_env *                      const envP,
                     int                               const flags,
                     const char *                      const appname,
                     const char *                      const appversion,
                     const struct xmlrpc_clientparms * const clientparmsP,
                     unsigned int                      const parmSize,
                     xmlrpc_client **                  const clientPP);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_destroy(xmlrpc_client * const clientP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_transport_call2(
    xmlrpc_env *               const envP,
    xmlrpc_client *            const clientP,
    const xmlrpc_server_info * const serverP,
    xmlrpc_mem_block *         const callXmlP,
    xmlrpc_mem_block **        const respXmlPP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_call2(xmlrpc_env *               const envP,
                    struct xmlrpc_client *     const clientP,
                    const xmlrpc_server_info * const serverInfoP,
                    const char *               const methodName,
                    xmlrpc_value *             const paramArrayP,
                    xmlrpc_value **            const resultPP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_call2f(xmlrpc_env *    const envP,
                     xmlrpc_client * const clientP,
                     const char *    const serverUrl,
                     const char *    const methodName,
                     xmlrpc_value ** const resultPP,
                     const char *    const format,
                     ...);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_call2f_va(xmlrpc_env *               const envP,
                        xmlrpc_client *            const clientP,
                        const char *               const serverUrl,
                        const char *               const methodName,
                        const char *               const format,
                        xmlrpc_value **            const resultPP,
                        va_list                          args);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_event_loop_finish(xmlrpc_client * const clientP);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_event_loop_finish_timeout(xmlrpc_client * const clientP,
                                        unsigned long   const milliseconds);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_start_rpc(xmlrpc_env *               const envP,
                        struct xmlrpc_client *     const clientP,
                        const xmlrpc_server_info * const serverInfoP,
                        const char *               const methodName,
                        xmlrpc_value *             const paramArrayP,
                        xmlrpc_response_handler          responseHandler,
                        void *                     const userData);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_start_rpcf(xmlrpc_env *    const envP,
                         xmlrpc_client * const clientP,
                         const char *    const serverUrl,
                         const char *    const methodName,
                         xmlrpc_response_handler responseHandler,
                         void *          const userData,
                         const char *    const format,
                         ...);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_start_rpcf_va(xmlrpc_env *    const envP,
                            xmlrpc_client * const clientP,
                            const char *    const serverUrl,
                            const char *    const methodName,
                            xmlrpc_response_handler responseHandler,
                            void *          const userData,
                            const char *    const format,
                            va_list               args);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_set_interrupt(xmlrpc_client * const clientP,
                            int *           const interruptP);

#include <xmlrpc-c/client_global.h>

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


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _XMLRPC_CLIENT_H_ */
