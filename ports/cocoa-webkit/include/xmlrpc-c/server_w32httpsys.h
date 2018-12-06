/* Copyright (C) 2005 by Steven A. Bone, sbone@pobox.com. All rights reserved.
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

/* COMPILATION NOTE:
   Note that the Platform SDK headers and
   link libraries for Windows XP SP2 or newer are required to compile
   xmlrpc-c for this module.  If you are not using this server, it is 
   safe to exclude the xmlrpc_server_w32httpsys.c file from the xmlrpc
   project and these dependencies will not be required.  You can get the 
   latest platform SDK at 
   http://www.microsoft.com/msdownload/platformsdk/sdkupdate/
   Be sure after installation to choose the program to "register the PSDK
   directories with Visual Studio" so the newer headers are found.
*/

#ifndef  _XMLRPC_SERVER_HTTPSYS_H_
#define  _XMLRPC_SERVER_HTTPSYS_H_ 1

#include "xmlrpc-c/c_util.h"  /* For XMLRPC_DLLEXPORT */
#include "transport_config.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
  XMLRPC_SERVER_HTTPSYS_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_server_httpsys.

  XMLRPC_BUILDING_SERVER_HTTPSYS says this compilation is part of
  libxmlrpc_server_httpsys, as opposed to something that _uses_
  libxmlrpc_server_httpsys.
*/
#ifdef XMLRPC_BUILDING_SERVER_HTTPSYS
#define XMLRPC_SERVER_HTTPSYS_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_SERVER_HTTPSYS_EXPORTED
#endif

/*=========================================================================
**  XML-RPC Server (based on HTTP.SYS)
**=========================================================================
**  A simple XML-RPC server based on the "built-in" Windows web server,
**  HTTP.SYS.  This is provided by Microsoft in Windows XP SP2 and 
**  Windows Server 2003.  If errors occur during server setup, the server
**  will exit. In general, if you want to use this API, you do not really
**  need to be familiar with the HTTP.SYS API.
*/

typedef void (*authorization_function)(
				 xmlrpc_env * envP,
                 char * userid,
                 char * password);

typedef struct {
    xmlrpc_registry * registryP;
	unsigned int	  portNum;
	unsigned int	  useSSL;
	/* useSSL, 0 = no SSL, 1 = use SSL */
	unsigned int	  logLevel;
	/* logLevel, 0 = none, 1 = file, 2 = file+OutputDebugString() */
	const char *      logFile;
	/* logFile, NULL or filename */
	authorization_function authfn;
} xmlrpc_server_httpsys_parms;

#define XMLRPC_HSSIZE(MBRNAME) \
    XMLRPC_STRUCTSIZE(xmlrpc_server_httpsys_parms, MBRNAME)

/* XMLRPC_HSSIZE(xyz) is the minimum size a struct xmlrpc_server_httpsys_parms
   must be to include the 'xyz' member.  This is essential for forward and
   backward compatbility, as new members will be added to the end of the
   struct in future releases.  This is how the callee knows whether or
   not the caller is new enough to have supplied a certain parameter.
*/

XMLRPC_SERVER_HTTPSYS_EXPORTED
void
xmlrpc_server_httpsys(
	xmlrpc_env *                        const envP,
    const xmlrpc_server_httpsys_parms * const parmsP,
    unsigned int                        const parm_size
	);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
