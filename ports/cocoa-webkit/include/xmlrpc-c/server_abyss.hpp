/*============================================================================
                              server_abyss.hpp
==============================================================================
  This declares the user interface to libxmlrpc_server_abyss++, which
  provides facilities for running a C++ XML-RPC server based on the Xmlrpc-c
  Abyss HTTP server.

  Nothing may include this header file that also includes <winsock.h>,
  because it conflicts with this file's use of <winsock2.h>.  Furthermore,
  nothing including this file may include <windows.h> without previously
  defining WIN32_LEAN_AND_MEAN, because <windows.h> without that macro
  includes <winsock.h> automatically.
============================================================================*/
#ifndef SERVER_ABYSS_HPP_INCLUDED
#define SERVER_ABYSS_HPP_INCLUDED

#ifdef _WIN32
   /* See restrictions above on including <windows.h> and <winsock.h> */
#  include <winsock2.h>   // For XMLRPC_SOCKET (= SOCKET)
#  include <ws2tcpip.h>
#endif

#include <xmlrpc-c/config.h>  // For XMLRPC_SOCKET
#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/base.hpp>
#include <xmlrpc-c/registry.hpp>
#include <xmlrpc-c/abyss.h>

/*
  XMLRPC_SERVER_ABYSSPP_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_server_abyss++.

  XMLRPC_BUILDING_SERVER_ABYSSPP says this compilation is part of
  libxmlrpc_server_abyss++, as opposed to something that _uses_
  libxmlrpc_server_abyss++.
*/
#ifdef XMLRPC_BUILDING_SERVER_ABYSSPP
#define XMLRPC_SERVER_ABYSSPP_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_SERVER_ABYSSPP_EXPORTED
#endif

namespace xmlrpc_c {

struct serverAbyss_impl;

class XMLRPC_SERVER_ABYSSPP_EXPORTED serverAbyss {
    
public:
    struct constrOpt_impl;

    class XMLRPC_SERVER_ABYSSPP_EXPORTED constrOpt {
    public:
        constrOpt();
        ~constrOpt();

        constrOpt & registryPtr       (xmlrpc_c::registryPtr      const& arg);
        constrOpt & registryP         (const xmlrpc_c::registry * const& arg);
        constrOpt & socketFd          (XMLRPC_SOCKET  const& arg);
        constrOpt & portNumber        (unsigned int   const& arg);
        constrOpt & maxConn           (unsigned int   const& arg);
        constrOpt & maxConnBacklog    (unsigned int   const& arg);
        constrOpt & keepaliveTimeout  (unsigned int   const& arg);
        constrOpt & keepaliveMaxConn  (unsigned int   const& arg);
        constrOpt & timeout           (unsigned int   const& arg);
        constrOpt & dontAdvertise     (bool           const& arg);
        constrOpt & uriPath           (std::string    const& arg);
        constrOpt & chunkResponse     (bool           const& arg);
        constrOpt & allowOrigin       (std::string    const& arg);
        constrOpt & accessCtlMaxAge   (unsigned int   const& arg);
        constrOpt & sockAddrP         (const struct sockaddr * const& arg);
        constrOpt & sockAddrLen       (socklen_t      const& arg);
        constrOpt & logFileName       (std::string    const& arg);
        constrOpt & serverOwnsSignals (bool           const& arg);
        constrOpt & expectSigchld     (bool           const& arg);

    private:
        struct constrOpt_impl * implP;
        friend class serverAbyss;
    };

    serverAbyss(constrOpt const& opt);

    serverAbyss(
        xmlrpc_c::registry const& registry,
        unsigned int       const  portNumber = 8080,
        std::string        const& logFileName = "",
        unsigned int       const  keepaliveTimeout = 0,
        unsigned int       const  keepaliveMaxConn = 0,
        unsigned int       const  timeout = 0,
        bool               const  dontAdvertise = false,
        bool               const  socketBound = false,
        XMLRPC_SOCKET      const  socketFd = 0
        );
    ~serverAbyss();
    
    void
    run();

    void
    runOnce();

    void
    runConn(int const socketFd);

#ifndef _WIN32
    void
    sigchld(pid_t pid);
#endif

    void
    terminate();
    
    class XMLRPC_SERVER_ABYSSPP_EXPORTED shutdown :
         public xmlrpc_c::registry::shutdown {
    public:
        shutdown(xmlrpc_c::serverAbyss * const severAbyssP);
        virtual ~shutdown();
        void doit(std::string const& comment, void * const callInfo) const;
    private:
        xmlrpc_c::serverAbyss * const serverAbyssP;
    };

private:

    serverAbyss_impl * implP;

    void
    initialize(constrOpt const& opt);
};

class XMLRPC_SERVER_ABYSSPP_EXPORTED callInfo_serverAbyss :
        public xmlrpc_c::callInfo {
/*----------------------------------------------------------------------------
   This is information about how an XML-RPC call arrived via an Abyss server.
   It is available to the user's XML-RPC method execute() method, so for
   example an XML-RPC method might execute differently depending upon the
   IP address of the client.

   This is for a user of a xmlrpc_c::serverAbyss server.
-----------------------------------------------------------------------------*/
public:
    callInfo_serverAbyss(xmlrpc_c::serverAbyss * const abyssServerP,
                         TSession *              const abyssSessionP);

    xmlrpc_c::serverAbyss * const serverAbyssP;
        // The server that is processing the RPC.
    TSession * const abyssSessionP;
        // The HTTP transaction that embodies the RPC.  You can ask this
        // object things like what the IP address of the client is.
};

class XMLRPC_SERVER_ABYSSPP_EXPORTED callInfo_abyss : public xmlrpc_c::callInfo {
/*----------------------------------------------------------------------------
   This is information about how an XML-RPC call arrived via an Abyss server.
   It is available to the user's XML-RPC method execute() method, so for
   example an XML-RPC method might execute differently depending upon the
   IP address of the client.

   This is for a user with his own Abyss server, using
   the "set_handlers" routines to make it into an XML-RPC server.
-----------------------------------------------------------------------------*/
public:
    callInfo_abyss(TSession * const abyssSessionP);

    TSession * abyssSessionP;
        // The HTTP transaction that embodies the RPC.  You can ask this
        // object things like what the IP address of the client is.
};

XMLRPC_SERVER_ABYSSPP_EXPORTED
void
server_abyss_set_handlers(TServer *          const  srvP,
                          xmlrpc_c::registry const& registry,
                          std::string        const& uriPath = "/RPC2");

XMLRPC_SERVER_ABYSSPP_EXPORTED
void
server_abyss_set_handlers(TServer *                  const  srvP,
                          const xmlrpc_c::registry * const  registryP,
                          std::string                const& uriPath = "/RPC2");

XMLRPC_SERVER_ABYSSPP_EXPORTED
void
server_abyss_set_handlers(TServer *             const srvP,
                          xmlrpc_c::registryPtr const registryPtr,
                          std::string           const& uriPath = "/RPC2");

} // namespace

#endif
