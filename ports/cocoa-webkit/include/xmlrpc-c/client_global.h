#ifndef CLIENT_GLOBAL_H_INCLUDED
#define CLIENT_GLOBAL_H_INCLUDED

#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/client.h>

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

/*=========================================================================
**  Initialization and Shutdown
**=========================================================================
**  These routines initialize and terminate the XML-RPC client. If you're
**  already using libwww on your own, you can pass
**  XMLRPC_CLIENT_SKIP_LIBWWW_INIT to avoid initializing it twice.
*/

#define XMLRPC_CLIENT_NO_FLAGS         (0)
#define XMLRPC_CLIENT_SKIP_LIBWWW_INIT (1)

XMLRPC_CLIENT_EXPORTED
extern void
xmlrpc_client_init(int          const flags,
                   const char * const appname,
                   const char * const appversion);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_init2(xmlrpc_env *                      const env,
                    int                               const flags,
                    const char *                      const appname,
                    const char *                      const appversion,
                    const struct xmlrpc_clientparms * const clientparms,
                    unsigned int                      const parm_size);

XMLRPC_CLIENT_EXPORTED
extern void
xmlrpc_client_cleanup(void);

/*=========================================================================
**  xmlrpc_client_call
**=======================================================================*/

XMLRPC_CLIENT_EXPORTED
xmlrpc_value * 
xmlrpc_client_call(xmlrpc_env * const envP,
                   const char * const server_url,
                   const char * const method_name,
                   const char * const format,
                   ...);

XMLRPC_CLIENT_EXPORTED
xmlrpc_value * 
xmlrpc_client_call_params(xmlrpc_env *   const envP,
                          const char *   const serverUrl,
                          const char *   const methodName,
                          xmlrpc_value * const paramArrayP);

XMLRPC_CLIENT_EXPORTED
xmlrpc_value * 
xmlrpc_client_call_server(xmlrpc_env *               const envP,
                          const xmlrpc_server_info * const server,
                          const char *               const method_name,
                          const char *               const format, 
                          ...);

XMLRPC_CLIENT_EXPORTED
xmlrpc_value *
xmlrpc_client_call_server_params(
    xmlrpc_env *               const envP,
    const xmlrpc_server_info * const serverP,
    const char *               const method_name,
    xmlrpc_value *             const paramArrayP);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_transport_call(
    xmlrpc_env *               const envP,
    void *                     const reserved,  /* for client handle */
    const xmlrpc_server_info * const serverP,
    xmlrpc_mem_block *         const callXmlP,
    xmlrpc_mem_block **        const respXmlPP);


/*=========================================================================
**  xmlrpc_client_call_asynch
**=========================================================================
**  An asynchronous XML-RPC client.
*/

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_call_asynch(const char * const server_url,
                          const char * const method_name,
                          xmlrpc_response_handler responseHandler,
                          void *       const user_data,
                          const char * const format,
                          ...);

XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_call_server_asynch(xmlrpc_server_info * const server,
                                 const char *         const method_name,
                                 xmlrpc_response_handler responseHandler,
                                 void *               const user_data,
                                 const char *         const format,
                                 ...);

XMLRPC_CLIENT_EXPORTED
void
xmlrpc_client_call_asynch_params(const char *   const server_url,
                                 const char *   const method_name,
                                 xmlrpc_response_handler responseHandler,
                                 void *         const user_data,
                                 xmlrpc_value * const paramArrayP);
    
XMLRPC_CLIENT_EXPORTED
void 
xmlrpc_client_call_server_asynch_params(
    xmlrpc_server_info * const server,
    const char *         const method_name,
    xmlrpc_response_handler    responseHandler,
    void *               const user_data,
    xmlrpc_value *       const paramArrayP);
    
XMLRPC_CLIENT_EXPORTED
extern void
xmlrpc_client_event_loop_finish_asynch(void);

XMLRPC_CLIENT_EXPORTED
extern void
xmlrpc_client_event_loop_finish_asynch_timeout(
    unsigned long const milliseconds);

#endif
