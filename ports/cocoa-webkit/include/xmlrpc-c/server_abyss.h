/*============================================================================
                              server_abyss.h
==============================================================================
  This declares the user interface to libxmlrpc_server_abyss, which
  provides facilities for running an XML-RPC server based on the Xmlrpc-c
  Abyss HTTP server.

  Nothing may include this header file that also includes <winsock.h>,
  because it conflicts with this file's use of <winsock2.h>.  Furthermore,
  nothing including this file may include <windows.h> without previously
  defining WIN32_LEAN_AND_MEAN, because <windows.h> without that macro
  includes <winsock.h> automatically.
============================================================================*/

/* Copyright and license information is at the end of the file */

#ifndef  XMLRPC_SERVER_ABYSS_H_INCLUDED
#define  XMLRPC_SERVER_ABYSS_H_INCLUDED

#ifdef _WIN32
/* See restriction above concerning windows.h and winsock.h */
#  include <winsock2.h>  /* For XMLRPC_SOCKET (= SOCKET) */
#  include <ws2tcpip.h>
#endif

#include <xmlrpc-c/config.h>  /* For XMLRPC_SOCKET */
#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/abyss.h>
#include <xmlrpc-c/server.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
  XMLRPC_SERVER_ABYSS_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_server_abyss.

  XMLRPC_BUILDING_SERVER_ABYSS says this compilation is part of
  libxmlrpc_server_abyss, as opposed to something that _uses_
  libxmlrpc_server_abyss.
*/
#ifdef XMLRPC_BUILDING_SERVER_ABYSS
#define XMLRPC_SERVER_ABYSS_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_SERVER_ABYSS_EXPORTED
#endif

#define XMLRPC_SERVER_ABYSS_NO_FLAGS (0)


/*=========================================================================
**  Global Initialization/Termination.
**  
**  These are not thread-safe.  You call them at the beginning and end
**  of your program, when it is only one thread.
**=======================================================================*/

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_global_init(xmlrpc_env * const envP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_global_term(void);

/*=========================================================================
**  Basic Abyss Server Functions
**=======================================================================*/

typedef void ((*runfirstFn)(void *));

typedef struct {
    const char *      config_file_name;
        /* NULL to use preferred proper API-level interface */

    xmlrpc_registry * registryP;

    /* runfirstFn and runfirst_arg are meaningless when 
       config_file_name is NULL
    */
    runfirstFn        runfirst;
    void *            runfirst_arg;

    unsigned int      port_number;
    const char *      log_file_name;
    unsigned int      keepalive_timeout;
    unsigned int      keepalive_max_conn;
    unsigned int      timeout;
    xmlrpc_bool       dont_advertise;
    xmlrpc_bool       socket_bound;
    XMLRPC_SOCKET     socket_handle;
    const char *      uri_path;
    xmlrpc_bool       chunk_response;
    xmlrpc_bool       enable_shutdown;
    const char *      allow_origin;
    xmlrpc_bool       access_ctl_expires;
    unsigned int      access_ctl_max_age;
    const struct sockaddr * sockaddr_p;
    socklen_t         sockaddrlen;
    unsigned int      max_conn;
    unsigned int      max_conn_backlog;
} xmlrpc_server_abyss_parms;


#define XMLRPC_APSIZE(MBRNAME) \
    XMLRPC_STRUCTSIZE(xmlrpc_server_abyss_parms, MBRNAME)

/* XMLRPC_APSIZE(xyz) is the minimum size a struct xmlrpc_server_abyss_parms
   must be to include the 'xyz' member.  This is essential to forward and
   backward compatibility, as new members will be added to the end of the
   struct in future releases.  This is how the callee knows whether or
   not the caller is new enough to have supplied a certain parameter.
*/

/*=========================================================================
**  Simple server with Abyss under the covers
**=======================================================================*/

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss(xmlrpc_env *                      const envP,
                    const xmlrpc_server_abyss_parms * const parms,
                    unsigned int                      const parmSize);

/*=========================================================================
**  Object-oriented XML-RPC server with Abyss under the covers
**=======================================================================*/

typedef struct xmlrpc_server_abyss xmlrpc_server_abyss_t;

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_create(xmlrpc_env *                      const envP,
                           const xmlrpc_server_abyss_parms * const parmsP,
                           unsigned int                      const parmSize,
                           xmlrpc_server_abyss_t **          const serverPP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_destroy(xmlrpc_server_abyss_t * const serverP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_run_server(xmlrpc_env *            const envP,
                               xmlrpc_server_abyss_t * const serverP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_terminate(xmlrpc_env *            const envP,
                              xmlrpc_server_abyss_t * const serverP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_reset_terminate(xmlrpc_env *            const envP,
                                    xmlrpc_server_abyss_t * const serverP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_use_sigchld(xmlrpc_server_abyss_t * const serverP);


typedef struct xmlrpc_server_abyss_sig xmlrpc_server_abyss_sig;

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_setup_sig(
    xmlrpc_env *               const envP,
    xmlrpc_server_abyss_t *    const serverP,
    xmlrpc_server_abyss_sig ** const oldHandlersPP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_restore_sig(
    const xmlrpc_server_abyss_sig * const oldHandlersP);



/*=========================================================================
**  Functions to make an XML-RPC server out of your own Abyss server
**=======================================================================*/

typedef void
xmlrpc_call_processor(xmlrpc_env *        const envP,
                      void *              const processorArg,
                      const char *        const callXml,
                      size_t              const callXmlLen,
                      TSession *          const abyssSessionP,
                      xmlrpc_mem_block ** const responseXmlPP);

typedef struct {
    xmlrpc_call_processor * xml_processor;
    void *                  xml_processor_arg;
    size_t                  xml_processor_max_stack;
    const char *            uri_path;
    xmlrpc_bool             chunk_response;
    const char *            allow_origin;
        /* NULL means don't answer HTTP access control query */
    xmlrpc_bool             access_ctl_expires;
    unsigned int            access_ctl_max_age;
} xmlrpc_server_abyss_handler_parms;

#define XMLRPC_AHPSIZE(MBRNAME) \
    XMLRPC_STRUCTSIZE(xmlrpc_server_abyss_handler_parms, MBRNAME)

/* XMLRPC_AHPSIZE(xyz) is the minimum size a struct
   xmlrpc_server_abyss_handler_parms must be to include the 'xyz' member.
   This is essential to forward and backward compatibility, as new members
   will be added to the end of the struct in future releases.  This is how the
   callee knows whether or not the caller is new enough to have supplied a
   certain parameter.
*/


XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_set_handler3(
    xmlrpc_env *                              const envP,
    TServer *                                 const srvP,
    const xmlrpc_server_abyss_handler_parms * const parms,
    unsigned int                              const parmSize);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_set_handler2(
    TServer *         const srvP,
    const char *      const uriPath,
    xmlrpc_call_processor   xmlProcessor,
    void *            const xmlProcessorArg,
    size_t            const xmlProcessorMaxStackSize,
    xmlrpc_bool       const chunkResponse);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_set_handlers2(TServer *         const srvP,
                                  const char *      const filename,
                                  xmlrpc_registry * const registryP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_set_handlers(TServer *         const serverP,
                                 xmlrpc_registry * const registryP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_set_handler(xmlrpc_env *      const envP,
                                TServer *         const serverP,
                                const char *      const filename,
                                xmlrpc_registry * const registryP);

XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_set_default_handler(TServer * const serverP);

/*=========================================================================
**  Handy Abyss Extensions
**=======================================================================*/

/* These are functions that have nothing to do with Xmlrpc-c, but provide
   convenient Abyss services beyond those provided by the Abyss library.
*/

/* Start an Abyss webserver running (previously created and
** initialized).  Under Unix, this routine will attempt to do a
** detaching fork, drop root privileges (if any) and create a pid
** file.  Under Windows, this routine merely starts the server.  This
** routine never returns.
**
** Once you call this routine, it is illegal to modify the server any
** more, including changing any method registry.
*/
XMLRPC_SERVER_ABYSS_EXPORTED
void
xmlrpc_server_abyss_run(void);

/* Same as xmlrpc_server_abyss_run(), except you get to specify a "runfirst"
** function.  The server runs this just before executing the actual server
** function, after any daemonizing.  NULL for 'runfirst' means no runfirst
** function.  'runfirstArg' is the argument the server passes to the runfirst
** function.
**/
XMLRPC_SERVER_ABYSS_EXPORTED
void 
xmlrpc_server_abyss_run_first(runfirstFn const runfirst,
                              void *     const runfirstArg);

/*=========================================================================
**  Method Registry
**=========================================================================
   These functions are for the built-in xmlrpc_server_abyss registry.
   It's usually simpler to skip all this and use the regular method
   registry services (from xmlrpc_server.h) to build a registry and
   pass it to xmlrpc_server_abyss.
*/

/* Call this function to create a new Abyss webserver with the default
** options and the built-in method registry.  If you've already
** initialized Abyss using Abyss functions, you can instead call
** xmlrpc_server_abyss_init_registry() to make it an Xmlrpc-c server.
** Or use a regular method registry and call
** xmlrpc_server_abyss_set_handlers().
**/
XMLRPC_SERVER_ABYSS_EXPORTED
void 
xmlrpc_server_abyss_init(int          const flags, 
                         const char * const config_file);

/* This is called automatically by xmlrpc_server_abyss_init. */
XMLRPC_SERVER_ABYSS_EXPORTED
void xmlrpc_server_abyss_init_registry (void);

/* Fetch the internal registry, if you happen to need it. 
   If you're using this, you really shouldn't be using the built-in
   registry at all.  It exists today only for backward compatibilty.
*/
XMLRPC_SERVER_ABYSS_EXPORTED
extern xmlrpc_registry *
xmlrpc_server_abyss_registry (void);

/* A quick & easy shorthand for adding a method. Depending on
** how you've configured your copy of Abyss, it's probably not safe to
** call this method after calling xmlrpc_server_abyss_run. */
XMLRPC_SERVER_ABYSS_EXPORTED
void xmlrpc_server_abyss_add_method (char *        const method_name,
                                     xmlrpc_method const method,
                                     void *        const user_data);
    
/* As above, but provide documentation (see xmlrpc_registry_add_method_w_doc
** for more information). You should really use this one. */
XMLRPC_SERVER_ABYSS_EXPORTED
extern void
xmlrpc_server_abyss_add_method_w_doc (char *        const method_name,
                                      xmlrpc_method const method,
                                      void *        const user_data,
                                      char *        const signature,
                                      char *        const help);

/*=========================================================================
**  Content Handlers
**=======================================================================*/
/* Abyss contents handlers xmlrpc_server_abyss_rpc2_handler()
   and xmlrpc_server_abyss_default_handler() were available in older
   Xmlrpc-c, but starting with Release 1.01, they are not.  Instead,
   call xmlrpc_server_abyss_set_handlers() to install them.

   Alternatively, you can write your own handlers that do the same thing.
   It's not hard, and if you're writing low enough level Abyss code that
   you can't use xmlrpc_server_abyss_set_handlers(), you probably want to
   anyway.
*/

#ifdef __cplusplus
}
#endif /* __cplusplus */

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
