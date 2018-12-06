/* Copyright information is at the end of the file */
#ifndef  XMLRPC_TRANSPORT_H_INCLUDED
#define  XMLRPC_TRANSPORT_H_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#include <xmlrpc-c/util.h>
#include <xmlrpc-c/client.h>

struct xmlrpc_call_info;

struct xmlrpc_client_transport;

/*=========================================================================
**  Transport function type declarations.
**=========================================================================
*/
typedef void (*xmlrpc_transport_setup)(xmlrpc_env * const envP);

typedef void (*xmlrpc_transport_teardown)(void);

typedef void (*xmlrpc_transport_create)(
    xmlrpc_env *                      const envP,
    int                               const flags,
    const char *                      const appname,
    const char *                      const appversion,
    const void *                      const transportparmsP,
    size_t                            const transportparm_size,
    struct xmlrpc_client_transport ** const handlePP);
    
typedef void (*xmlrpc_transport_destroy)(
    struct xmlrpc_client_transport * const clientTransportP);

typedef void (*xmlrpc_transport_asynch_complete)(
    struct xmlrpc_call_info * const callInfoP,
    xmlrpc_mem_block *        const responseXmlP,
    xmlrpc_env                const env);

typedef void (*xmlrpc_transport_progress)(
    struct xmlrpc_call_info *   const callInfoP,
    struct xmlrpc_progress_data const data);

typedef void (*xmlrpc_transport_send_request)(
    xmlrpc_env *                     const envP, 
    struct xmlrpc_client_transport * const clientTransportP,
    const xmlrpc_server_info *       const serverP,
    xmlrpc_mem_block *               const xmlP,
    xmlrpc_transport_asynch_complete       complete,
    xmlrpc_transport_progress              progress,
    struct xmlrpc_call_info *        const callInfoP);

typedef void (*xmlrpc_transport_call)(
    xmlrpc_env *                     const envP,
    struct xmlrpc_client_transport * const clientTransportP,
    const xmlrpc_server_info *       const serverP,
    xmlrpc_mem_block *               const xmlP,
    xmlrpc_mem_block **              const responsePP);

typedef enum {timeout_no, timeout_yes} xmlrpc_timeoutType;

typedef unsigned long xmlrpc_timeout;
    /* A timeout in milliseconds. */

typedef void (*xmlrpc_transport_finish_asynch)(
    struct xmlrpc_client_transport * const clientTransportP,
    xmlrpc_timeoutType               const timeoutType,
    xmlrpc_timeout                   const timeout);

typedef void (*xmlrpc_transport_set_interrupt)(
    struct xmlrpc_client_transport * const clientTransportP,
    int *                            const interruptP);

struct xmlrpc_client_transport_ops {

    xmlrpc_transport_setup         setup_global_const;
    xmlrpc_transport_teardown      teardown_global_const;
    xmlrpc_transport_create        create;
    xmlrpc_transport_destroy       destroy;
    xmlrpc_transport_send_request  send_request;
    xmlrpc_transport_call          call;
    xmlrpc_transport_finish_asynch finish_asynch;
    xmlrpc_transport_set_interrupt set_interrupt;
};

extern int xmlrpc_trace_transport;
    // This is nonzero to indicate that client XML transport logic should
    // be traced.

#ifdef __cplusplus
}
#endif

#endif
