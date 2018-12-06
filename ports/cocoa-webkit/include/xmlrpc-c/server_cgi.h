/* Interface header file for libxmlrpc_server_cgi.

   By Bryan Henderson, 05.04.27.  Contributed to the public domain.
*/

#ifndef  XMLRPC_CGI_H_INCLUDED
#define  XMLRPC_CGI_H_INCLUDED

#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/server.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
  XMLRPC_SERVER_CGI_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_server_cgi.

  XMLRPC_BUILDING_SERVER_CGI says this compilation is part of
  libxmlrpc_server_cgi, as opposed to something that _uses_
  libxmlrpc_server_cgi.
*/
#ifdef XMLRPC_BUILDING_SERVER_CGI
#define XMLRPC_SERVER_CGI_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_SERVER_CGI_EXPORTED
#endif

XMLRPC_SERVER_CGI_EXPORTED
void
xmlrpc_server_cgi_process_call(xmlrpc_registry * const registryP);

#define XMLRPC_CGI_NO_FLAGS (0)

XMLRPC_SERVER_CGI_EXPORTED
extern void
xmlrpc_cgi_init(int const flags);

XMLRPC_SERVER_CGI_EXPORTED
extern xmlrpc_registry *
xmlrpc_cgi_registry (void);

XMLRPC_SERVER_CGI_EXPORTED
void
xmlrpc_cgi_add_method(const char *  const method_name,
                      xmlrpc_method const method,
                      void *        const user_data);

XMLRPC_SERVER_CGI_EXPORTED
void
xmlrpc_cgi_add_method_w_doc(const char *  const method_name,
                            xmlrpc_method const method,
                            void *        const user_data,
                            const char *  const signature,
                            const char *  const help);
XMLRPC_SERVER_CGI_EXPORTED
extern void
xmlrpc_cgi_process_call (void);

XMLRPC_SERVER_CGI_EXPORTED
extern void
xmlrpc_cgi_cleanup (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
