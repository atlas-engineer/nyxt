/*****************************************************************************
                                 abyss.h
******************************************************************************

  This file is the interface header for the Abyss HTTP server component of
  XML-RPC For C/C++ (Xmlrpc-c).

  The Abyss component of Xmlrpc-c is based on the independently developed
  and distributed Abyss web server package from 2001.

  Nothing may include this header file that also includes <winsock.h>,
  because it conflicts with this file's use of <winsock2.h>.  Furthermore,
  nothing including this file may include <windows.h> without previously
  defining WIN32_LEAN_AND_MEAN, because <windows.h> without that macro
  includes <winsock.h> automatically.  (see abyss_winsock.h).

  Copyright information is at the end of the file.
****************************************************************************/

#ifndef XMLRPC_ABYSS_H_INCLUDED
#define XMLRPC_ABYSS_H_INCLUDED


#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>

#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/inttypes.h>

/*
  XMLRPC_ABYSS_EXPORTED marks a symbol in this file that is exported from
  libxmlrpc_abyss.

  XMLRPC_BUILDING_ABYSS says this compilation is part of libxmlrpc_abyss, as
  opposed to something that _uses_ libxmlrpc_abyss.
*/
#ifdef XMLRPC_BUILDING_ABYSS
#define XMLRPC_ABYSS_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_ABYSS_EXPORTED
#endif

/****************************************************************************
  STUFF FOR THE OUTER CONTROL PROGRAM TO USE
****************************************************************************/

typedef int abyss_bool;

/****************************************************************************
  GLOBAL (STATIC) PROGRAM STUFF
****************************************************************************/

XMLRPC_ABYSS_EXPORTED
void
AbyssInit(const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
void
AbyssTerm(void);

/*********************************************************************
** MIMEType
*********************************************************************/

typedef struct MIMEType MIMEType;

XMLRPC_ABYSS_EXPORTED
MIMEType *
MIMETypeCreate(void);

XMLRPC_ABYSS_EXPORTED
void
MIMETypeDestroy(MIMEType * const MIMETypeP);

XMLRPC_ABYSS_EXPORTED
void
MIMETypeInit(void);

XMLRPC_ABYSS_EXPORTED
void
MIMETypeTerm(void);

XMLRPC_ABYSS_EXPORTED
abyss_bool
MIMETypeAdd2(MIMEType *   const MIMETypeP,
             const char * const type,
             const char * const ext);

XMLRPC_ABYSS_EXPORTED
abyss_bool
MIMETypeAdd(const char * const type,
            const char * const ext);


enum abyss_foreback {ABYSS_FOREGROUND, ABYSS_BACKGROUND};

#define HAVE_CHANSWITCH

typedef struct _TChanSwitch TChanSwitch;
typedef struct _TChannel TChannel;
typedef struct _TSocket TSocket;

#ifdef _WIN32
  #include <xmlrpc-c/abyss_winsock.h>
#else
  #include <xmlrpc-c/abyss_unixsock.h>
#endif

XMLRPC_ABYSS_EXPORTED
void
ChanSwitchInit(const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
void
ChanSwitchTerm(void);

/* If you're wondering where the constructors for TChanSwitch,
   TChannel, and TSocket are: They're implementation-specific, so look
   in abyss_unixsock.h, etc.
*/

XMLRPC_ABYSS_EXPORTED
void
ChanSwitchDestroy(TChanSwitch * const chanSwitchP);

XMLRPC_ABYSS_EXPORTED
void
ChannelInit(const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
void
ChannelTerm(void);

XMLRPC_ABYSS_EXPORTED
void
ChannelDestroy(TChannel * const channelP);

XMLRPC_ABYSS_EXPORTED
void
SocketDestroy(TSocket * const socketP);


typedef struct {
    /* Before Xmlrpc-c 1.04, the internal server representation,
       struct _TServer, was exposed to users and was the only way to
       set certain parameters of the server.  Now, use the (new)
       ServerSet...() functions.  Use the HAVE_ macros to determine
       which method you have to use.
    */
    struct _TServer * srvP;
} TServer;

typedef struct _TSession TSession;

XMLRPC_ABYSS_EXPORTED
abyss_bool
ServerCreate(TServer *       const serverP,
             const char *    const name,
             xmlrpc_uint16_t const port,
             const char *    const filespath,
             const char *    const logfilename);

XMLRPC_ABYSS_EXPORTED
void
ServerCreateSwitch(TServer *     const serverP,
                   TChanSwitch * const chanSwitchP,
                   const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ServerCreateSocket(TServer *    const serverP,
                   const char * const name,
                   TOsSocket    const socketFd,
                   const char * const filespath,
                   const char * const logfilename);

#define HAVE_SERVER_CREATE_SOCKET_2
XMLRPC_ABYSS_EXPORTED
void
ServerCreateSocket2(TServer *     const serverP,
                    TSocket *     const socketP,
                    const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ServerCreateNoAccept(TServer *    const serverP,
                     const char * const name,
                     const char * const filespath,
                     const char * const logfilename);

XMLRPC_ABYSS_EXPORTED
void
ServerFree(TServer * const serverP);

XMLRPC_ABYSS_EXPORTED
void
ServerSetName(TServer *    const serverP,
              const char * const name);

XMLRPC_ABYSS_EXPORTED
void
ServerSetFilesPath(TServer *    const serverP,
                   const char * const filesPath);

XMLRPC_ABYSS_EXPORTED
void
ServerSetLogFileName(TServer *    const serverP,
                     const char * const logFileName);

#define HAVE_SERVER_SET_KEEPALIVE_TIMEOUT 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetKeepaliveTimeout(TServer *       const serverP,
                          xmlrpc_uint32_t const keepaliveTimeout);

#define HAVE_SERVER_SET_KEEPALIVE_MAX_CONN 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetKeepaliveMaxConn(TServer *       const serverP,
                          xmlrpc_uint32_t const keepaliveMaxConn);

#define HAVE_SERVER_SET_TIMEOUT 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetTimeout(TServer *       const serverP,
                 xmlrpc_uint32_t const timeout);

#define HAVE_SERVER_SET_ADVERTISE 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetAdvertise(TServer *  const serverP,
                   abyss_bool const advertise);

#define HAVE_SERVER_SET_MIME_TYPE 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetMimeType(TServer *  const serverP,
                  MIMEType * const MIMETypeP);

#define HAVE_SERVER_SET_MAX_CONN 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetMaxConn(TServer *    const serverP,
                 unsigned int const maxConn);

#define HAVE_SERVER_SET_MAX_CONN_BACKLOG 1
XMLRPC_ABYSS_EXPORTED
void
ServerSetMaxConnBacklog(TServer *    const serverP,
                        unsigned int const maxConnBacklog);

XMLRPC_ABYSS_EXPORTED
void
ServerInit2(TServer *     const serverP,
            const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
void
ServerInit(TServer * const serverP);

XMLRPC_ABYSS_EXPORTED
void
ServerRun(TServer * const serverP);

XMLRPC_ABYSS_EXPORTED
void
ServerRunOnce(TServer * const serverP);

/* ServerRunOnce2() is obsolete.  See user's guide. */
XMLRPC_ABYSS_EXPORTED
void
ServerRunOnce2(TServer *           const serverP,
               enum abyss_foreback const foregroundBackground);

XMLRPC_ABYSS_EXPORTED
void
ServerRunChannel(TServer *     const serverP,
                 TChannel *    const channelP,
                 void *        const channelInfoP,
                 const char ** const errorP);

#define HAVE_SERVER_RUN_CONN_2
XMLRPC_ABYSS_EXPORTED
void
ServerRunConn2(TServer *     const serverP,
               TSocket *     const connectedSocketP,
               const char ** const errorP);

XMLRPC_ABYSS_EXPORTED
void
ServerRunConn(TServer * const serverP,
              TOsSocket const connectedSocket);

XMLRPC_ABYSS_EXPORTED
void
ServerDaemonize(TServer * const serverP);

XMLRPC_ABYSS_EXPORTED
void
ServerTerminate(TServer * const serverP);

XMLRPC_ABYSS_EXPORTED
void
ServerResetTerminate(TServer * const serverP);

XMLRPC_ABYSS_EXPORTED
void
ServerUseSigchld(TServer * const serverP);

#ifndef _WIN32
void
ServerHandleSigchld(pid_t const pid);
#endif

typedef abyss_bool (*URIHandler) (TSession *); /* deprecated */

struct URIHandler2;

typedef void (*initHandlerFn)(struct URIHandler2 *, abyss_bool *);

typedef void (*termHandlerFn)(void *);

typedef void (*handleReq3Fn)(void *,
                             TSession *,
                             abyss_bool *);

typedef void (*handleReq2Fn)(struct URIHandler2 *,
                             TSession *,
                             abyss_bool *);

struct ServerReqHandler3 {
    termHandlerFn term;
    handleReq3Fn  handleReq;
    void *        userdata;
    size_t        handleReqStackSize; /* zero = default */
};

XMLRPC_ABYSS_EXPORTED
void
ServerAddHandler3(TServer *                        const serverP,
                  const struct ServerReqHandler3 * const handlerP,
                  abyss_bool *                     const successP);

typedef struct URIHandler2 {
    initHandlerFn init;
    termHandlerFn term;
    handleReq2Fn  handleReq2;
    URIHandler    handleReq1;  /* deprecated */
    void *        userdata;
} URIHandler2;

XMLRPC_ABYSS_EXPORTED
void
ServerAddHandler2(TServer *     const srvP,
                  URIHandler2 * const handlerP,
                  abyss_bool *  const successP);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ServerAddHandler(TServer * const srvP,
                 URIHandler const handler);

typedef abyss_bool (*THandlerDflt) (TSession *);

/* Note: 'handler' used to be URIHandler;  THandlerDflt is a newer name
   for the same type
*/

XMLRPC_ABYSS_EXPORTED
void
ServerDefaultHandler(TServer *    const srvP,
                     THandlerDflt const handler);

/* ConfReadServerFile() is inappropriately named; it was a mistake.
   But then, so is having this function at all.  The config file is
   inappropriate for an API.
*/

XMLRPC_ABYSS_EXPORTED
abyss_bool
ConfReadServerFile(const char * const filename,
                   TServer *    const srvP);

XMLRPC_ABYSS_EXPORTED
void
LogWrite(TServer *    const srvP,
         const char * const c);

/****************************************************************************
  STUFF FOR HTTP REQUEST HANDLERS TO USE
****************************************************************************/

typedef enum {
    m_unknown, m_get, m_put, m_head, m_post, m_delete, m_trace, m_options
} TMethod;

typedef struct {
    TMethod method;
    const char * uri;
        /* This is NOT the URI.  It is the pathname part of the URI.
           We really should fix that and put the pathname in another
           member.  If the URI does not contain a pathname, this is "*".
        */
    const char * query;
        /* The query part of the URI (stuff after '?').  NULL if none. */
    const char * host;
        /* NOT the value of the host: header field.  Rather, the name of the
           target host (could be part of the host: value; could be from the
           URI).  No port number.  NULL if request does not specify a host
           name.
        */
    const char * from;
    const char * useragent;
    const char * referer;
    const char * requestline;
    const char * user;
        /* Requesting user (from authorization: header field).  NULL if
           request doesn't specify or handler has not authenticated it.
        */
    xmlrpc_uint16_t port;
        /* The port number from the URI, or default 80 if the URI doesn't
           specify a port.
        */
    abyss_bool keepalive;
} TRequestInfo;

XMLRPC_ABYSS_EXPORTED
abyss_bool
SessionRefillBuffer(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
size_t
SessionReadDataAvail(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
void
SessionGetReadData(TSession *    const sessionP, 
                   size_t        const max, 
                   const char ** const outStartP, 
                   size_t *      const outLenP);

XMLRPC_ABYSS_EXPORTED
void
SessionGetRequestInfo(TSession *            const sessionP,
                      const TRequestInfo ** const requestInfoPP);

XMLRPC_ABYSS_EXPORTED
void
SessionGetChannelInfo(TSession * const sessionP,
                      void **    const channelInfoPP);

XMLRPC_ABYSS_EXPORTED
void *
SessionGetDefaultHandlerCtx(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
char *
RequestHeaderValue(TSession *   const sessionP,
                   const char * const name);

XMLRPC_ABYSS_EXPORTED
abyss_bool
RequestAuth(TSession *   const sessionP,
            const char * const credential,
            const char * const user,
            const char * const pass);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ResponseAddField(TSession *   const sessionP,
                 const char * const name,
                 const char * const value);

XMLRPC_ABYSS_EXPORTED
void
ResponseWriteStart(TSession * const sessionP);

/* For backward compatibility: */
#define ResponseWrite ResponseWriteStart

XMLRPC_ABYSS_EXPORTED
abyss_bool
ResponseWriteBody(TSession *      const sessionP,
                  const char *    const data,
                  xmlrpc_uint32_t const len);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ResponseWriteEnd(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ResponseChunked(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
xmlrpc_uint16_t
ResponseStatusFromErrno(int const errnoArg);

XMLRPC_ABYSS_EXPORTED
void
ResponseStatus(TSession *      const sessionP,
               xmlrpc_uint16_t const code);

XMLRPC_ABYSS_EXPORTED
void
ResponseStatusErrno(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ResponseContentType(TSession *   const serverP,
                    const char * const type);

XMLRPC_ABYSS_EXPORTED
abyss_bool
ResponseContentLength(TSession *      const sessionP,
                      xmlrpc_uint64_t const len);

typedef struct {
/*----------------------------------------------------------------------------
   These are parameters to control the HTTP "Access Control" functions.  That's
   where the client asks whether it is OK to send a request that some other
   server asked the client to send (e.g. a person web browses a page at
   a.example.com, and it sends a script that executes on the user's computer
   and tries to perform an XML-RPC RPC on b.example.com.  The user's browser
   first asks b.example.com if it is OK to do an RPC that is really initiated
   by a.example.com.

   These parameters tell the server how to respond to such a request.
-----------------------------------------------------------------------------*/
    const char * allowOrigin;
        /* This tells what original servers (a.example.com in the example
           above) are allowed to submit RPCs indirectly to us.  The value is a
           verbatim value for an HTTP Access-Control-Allow-Origin header field
           (just the value part of the field, not the whole field).  "*"
           therefore means everyone is allowed.  "" means no one.
    
           NULL means not to say anything about access control to the client.
        */
    abyss_bool expires;
        /* The permissions herein expire after a certain period from now.
           'maxAge' is that period.
        */
    unsigned int maxAge;
        /* Meaningful only when 'expires' is true.  The expiration period
           in seconds.  Zero is valid.
        */
} ResponseAccessCtl;

XMLRPC_ABYSS_EXPORTED
void
ResponseAccessControl(TSession *        const abyssSessionP, 
                      ResponseAccessCtl const accessControl);

XMLRPC_ABYSS_EXPORTED
void
ResponseError2(TSession *   const sessionP,
               const char * const explanation);

XMLRPC_ABYSS_EXPORTED
void
ResponseError(TSession * const sessionP);

XMLRPC_ABYSS_EXPORTED
const char *
MIMETypeFromExt(const char * const ext);

XMLRPC_ABYSS_EXPORTED
const char *
MIMETypeFromExt2(MIMEType *   const MIMETypeP,
                 const char * const ext);

XMLRPC_ABYSS_EXPORTED
const char *
MIMETypeFromFileName2(MIMEType *   const MIMETypeP,
                      const char * const fileName);

XMLRPC_ABYSS_EXPORTED
const char *
MIMETypeFromFileName(const char * const fileName);

XMLRPC_ABYSS_EXPORTED
const char *
MIMETypeGuessFromFile2(MIMEType *   const MIMETypeP,
                       const char * const fileName);

XMLRPC_ABYSS_EXPORTED
const char *
MIMETypeGuessFromFile(const char * const filename);


/****************************************************************************
  STUFF THAT PROBABLY DOESN'T BELONG IN THIS FILE BECAUSE IT IS INTERNAL

  Some day, we sort this out.
****************************************************************************/


/*********************************************************************
** Paths and so on...
*********************************************************************/

#ifdef _WIN32
#define DEFAULT_ROOT        "c:\\abyss"
#define DEFAULT_DOCS        DEFAULT_ROOT"\\htdocs"
#define DEFAULT_CONF_FILE   DEFAULT_ROOT"\\conf\\abyss.conf"
#define DEFAULT_LOG_FILE    DEFAULT_ROOT"\\log\\abyss.log"
#else
#ifdef __rtems__
#define DEFAULT_ROOT        "/abyss"
#else
#define DEFAULT_ROOT        "/usr/local/abyss"
#endif
#define DEFAULT_DOCS        DEFAULT_ROOT"/htdocs"
#define DEFAULT_CONF_FILE   DEFAULT_ROOT"/conf/abyss.conf"
#define DEFAULT_LOG_FILE    DEFAULT_ROOT"/log/abyss.log"
#endif

/*********************************************************************
** Range
*********************************************************************/

XMLRPC_ABYSS_EXPORTED
abyss_bool
RangeDecode(char *            const str,
            xmlrpc_uint64_t   const filesize,
            xmlrpc_uint64_t * const start,
            xmlrpc_uint64_t * const end);

XMLRPC_ABYSS_EXPORTED
abyss_bool DateInit(void);

/*********************************************************************
** Session
*********************************************************************/

XMLRPC_ABYSS_EXPORTED
abyss_bool SessionLog(TSession * const s);


#ifdef __cplusplus
}


#endif

/*****************************************************************************
** Here is the copyright notice from the Abyss web server project file from
** which this file is derived.
**
** Copyright (C) 2000 by Moez Mahfoudh <mmoez@bigfoot.com>.
** All rights reserved.
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
** SUCH DAMAGE.
**
******************************************************************************/
#endif  /* _ABYSS_H_ */
