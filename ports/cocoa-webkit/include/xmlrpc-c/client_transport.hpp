#ifndef CLIENT_TRANSPORT_HPP_INCLUDED
#define CLIENT_TRANSPORT_HPP_INCLUDED

#include <string>
#include <vector>

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/util.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/girerr.hpp>
#include <xmlrpc-c/girmem.hpp>
#include <xmlrpc-c/timeout.hpp>

namespace xmlrpc_c {

/*
  XMLRPC_CLIENTPP_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_client++.

  XMLRPC_BUILDING_CLIENTPP says this compilation is part of
  libxmlrpc_client++, as opposed to something that _uses_ libxmlrpc_client++.
*/
#ifdef XMLRPC_BUILDING_CLIENTPP
#define XMLRPC_CLIENTPP_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_CLIENTPP_EXPORTED
#endif

class XMLRPC_CLIENTPP_EXPORTED carriageParmPtr;

class XMLRPC_CLIENTPP_EXPORTED carriageParm : public girmem::autoObject {
/*----------------------------------------------------------------------------
   The parameter to a client for an individual RPC.  It tells specifics
   of how to carry the call to the server and the response back.  For
   example, it may identify the server.  It may identify communication
   protocols to use.  It may indicate permission and accounting
   information.

   This is a base class; the carriage parameter is specific to the
   class of client.  For example, an HTTP-based client would have a
   URL and HTTP basic authentication info as parameter.
-----------------------------------------------------------------------------*/
protected:
    virtual ~carriageParm();
    carriageParm();
};

class XMLRPC_CLIENTPP_EXPORTED carriageParmPtr : public girmem::autoObjectPtr {

public:
    carriageParmPtr();

    explicit carriageParmPtr(xmlrpc_c::carriageParm * const carriageParmP);

    xmlrpc_c::carriageParm *
    operator->() const;

    xmlrpc_c::carriageParm *
    get() const;
};

//----------------------------------------------------------------------------

class XMLRPC_CLIENTPP_EXPORTED xmlTransactionPtr;

class XMLRPC_CLIENTPP_EXPORTED xmlTransaction : public girmem::autoObject {

    friend class xmlTransactionPtr;

public:
    virtual void
    finish(std::string const& responseXml) const;

    virtual void
    finishErr(girerr::error const& error) const;

    virtual void
    progress(struct xmlrpc_progress_data const& progressData) const;

protected:
    xmlTransaction();
};

class XMLRPC_CLIENTPP_EXPORTED xmlTransactionPtr : public girmem::autoObjectPtr {
public:
    xmlTransactionPtr();

    xmlTransactionPtr(xmlTransaction * xmlTransP);
 
    xmlrpc_c::xmlTransaction *
    operator->() const;
};

//----------------------------------------------------------------------------

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransport : public girmem::autoObject {
/*----------------------------------------------------------------------------
   An object which transports XML to and from an XML-RPC server for an
   XML-RPC client.

   This is a base class.  Derived classes define methods to perform the
   transportation in particular ways.
-----------------------------------------------------------------------------*/
public:
    virtual ~clientXmlTransport();

    virtual void
    call(xmlrpc_c::carriageParm * const  carriageParmP,
         std::string              const& callXml,
         std::string *            const  responseXmlP) = 0;

    virtual void
    start(xmlrpc_c::carriageParm *    const  carriageParmP,
          std::string                 const& callXml,
          xmlrpc_c::xmlTransactionPtr const& xmlTranP);

    virtual void
    finishAsync(xmlrpc_c::timeout const timeout);

    static void
    asyncComplete(
        struct xmlrpc_call_info * const callInfoP,
        xmlrpc_mem_block *        const responseXmlMP,
        xmlrpc_env                const transportEnv);

    static void
    progress(
        struct xmlrpc_call_info *   const callInfoP,
        struct xmlrpc_progress_data const progressData);

    virtual void
    setInterrupt(int * const interruptP);
};

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransportPtr : public girmem::autoObjectPtr {
    
public:
    clientXmlTransportPtr();

    clientXmlTransportPtr(xmlrpc_c::clientXmlTransport * const transportP);

    xmlrpc_c::clientXmlTransport *
    operator->() const;

    xmlrpc_c::clientXmlTransport *
    get() const;
};

/*===========================================================================
                           HTTP
===========================================================================*/

class XMLRPC_CLIENTPP_EXPORTED carriageParm_http0 : public xmlrpc_c::carriageParm {

public:
    carriageParm_http0(std::string const serverUrl);

    ~carriageParm_http0();

    void
    setUser(std::string const userid,
            std::string const password);

    void
    allowAuthBasic();

    void
    disallowAuthBasic();
            
    void
    allowAuthDigest();

    void
    disallowAuthDigest();
            
    void
    allowAuthNegotiate();

    void
    disallowAuthNegotiate();
            
    void
    allowAuthNtlm();

    void
    disallowAuthNtlm();
            
    void
    setBasicAuth(std::string const userid,
                 std::string const password);

    xmlrpc_server_info * c_serverInfoP;

protected:
    // Only a derived class is allowed to create an object with no
    // server URL, and the derived class is expected to follow it up
    // with an instantiate() to establish the server URL.

    carriageParm_http0();

    void
    instantiate(std::string const serverUrl);
};

class XMLRPC_CLIENTPP_EXPORTED carriageParm_http0Ptr : public xmlrpc_c::carriageParmPtr {

public:
    carriageParm_http0Ptr();
    carriageParm_http0Ptr(xmlrpc_c::carriageParm_http0 * const carriageParmP);

    xmlrpc_c::carriageParm_http0 *
    operator->() const;
};

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransport_http : public xmlrpc_c::clientXmlTransport {
/*----------------------------------------------------------------------------
   A base class for client XML transports that use the simple, classic
   C HTTP transports.
-----------------------------------------------------------------------------*/
public:
    virtual ~clientXmlTransport_http();
    
    void
    call(xmlrpc_c::carriageParm * const  carriageParmP,
         std::string              const& callXml,
         std::string *            const  responseXmlP);
    
    void
    start(xmlrpc_c::carriageParm *    const  carriageParmP,
          std::string                 const& callXml,
          xmlrpc_c::xmlTransactionPtr const& xmlTranP);
        
    virtual void
    finishAsync(xmlrpc_c::timeout const timeout);

    virtual void
    setInterrupt(int * const interruptP);

    static std::vector<std::string>
    availableTypes();

    static clientXmlTransportPtr
    create();

protected:
    clientXmlTransport_http() {} // ensure no one can create
    struct xmlrpc_client_transport *           c_transportP;
    const struct xmlrpc_client_transport_ops * c_transportOpsP;
};


/*===========================================================================
                           curl
===========================================================================*/

class XMLRPC_CLIENTPP_EXPORTED carriageParm_curl0 : public xmlrpc_c::carriageParm_http0 {

public:
    carriageParm_curl0(std::string const serverUrl);
};

class XMLRPC_CLIENTPP_EXPORTED carriageParm_curl0Ptr : public xmlrpc_c::carriageParm_http0Ptr {

public:
    carriageParm_curl0Ptr();
    carriageParm_curl0Ptr(xmlrpc_c::carriageParm_curl0 * const carriageParmP);

    xmlrpc_c::carriageParm_curl0 *
    operator->() const;
};

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransport_curl : public xmlrpc_c::clientXmlTransport_http {

public:
    struct constrOpt_impl;

    class XMLRPC_CLIENTPP_EXPORTED constrOpt {
    public:
        constrOpt();
        ~constrOpt();
        constrOpt(constrOpt&);

        constrOpt & network_interface (std::string  const& arg);
        constrOpt & no_ssl_verifypeer (bool         const& arg);
        constrOpt & no_ssl_verifyhost (bool         const& arg);
        constrOpt & dont_advertise    (bool         const& arg);
        constrOpt & user_agent        (std::string  const& arg);
        constrOpt & referer           (std::string  const& arg);
        constrOpt & ssl_cert          (std::string  const& arg);
        constrOpt & sslcerttype       (std::string  const& arg);
        constrOpt & sslcertpasswd     (std::string  const& arg);
        constrOpt & sslkey            (std::string  const& arg);
        constrOpt & sslkeytype        (std::string  const& arg);
        constrOpt & sslkeypasswd      (std::string  const& arg);
        constrOpt & sslengine         (std::string  const& arg);
        constrOpt & sslengine_default (bool         const& arg);
        constrOpt & sslversion        (xmlrpc_sslversion const& arg);
        constrOpt & cainfo            (std::string  const& arg);
        constrOpt & capath            (std::string  const& arg);
        constrOpt & randomfile        (std::string  const& arg);
        constrOpt & egdsocket         (std::string  const& arg);
        constrOpt & ssl_cipher_list   (std::string  const& arg);
        constrOpt & timeout           (unsigned int const& arg);
        constrOpt & proxy             (std::string  const& arg);
        constrOpt & proxy_port        (unsigned int const& arg);
        constrOpt & proxy_auth        (unsigned int const& arg);
        constrOpt & proxy_userpwd     (std::string  const& arg);
        constrOpt & proxy_type        (xmlrpc_httpproxytype const& arg);
        constrOpt & gssapi_delegation (bool         const& arg);

    private:
        struct constrOpt_impl * implP;
        friend class clientXmlTransport_curl;
    };

    clientXmlTransport_curl(constrOpt const& opt);

    clientXmlTransport_curl(std::string const networkInterface = "",
                            bool        const noSslVerifyPeer = false,
                            bool        const noSslVerifyHost = false,
                            std::string const userAgent = "");

    ~clientXmlTransport_curl();

private:
    void
    initialize(constrOpt const& opt);
};

/*===========================================================================
                           libwww
===========================================================================*/

class XMLRPC_CLIENTPP_EXPORTED carriageParm_libwww0 : public xmlrpc_c::carriageParm_http0 {

public:
    carriageParm_libwww0(std::string const serverUrl);

};

class XMLRPC_CLIENTPP_EXPORTED carriageParm_libwww0Ptr : public xmlrpc_c::carriageParm_http0Ptr {

public:
    carriageParm_libwww0Ptr();
    carriageParm_libwww0Ptr(xmlrpc_c::carriageParm_libwww0 * const);

    xmlrpc_c::carriageParm_libwww0 *
    operator->() const;
};

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransport_libwww : public xmlrpc_c::clientXmlTransport_http {
    
public:
    clientXmlTransport_libwww(std::string const appname = "",
                              std::string const appversion = "");

    ~clientXmlTransport_libwww();
};

/*===========================================================================
                           wininet
===========================================================================*/

class XMLRPC_CLIENTPP_EXPORTED carriageParm_wininet0 : public xmlrpc_c::carriageParm_http0 {

public:
    carriageParm_wininet0(std::string const serverUrl);

};

class XMLRPC_CLIENTPP_EXPORTED carriageParm_wininet0Ptr : public xmlrpc_c::carriageParm_http0Ptr {

public:
    carriageParm_wininet0Ptr();
    carriageParm_wininet0Ptr(xmlrpc_c::carriageParm_wininet0 * const);

    xmlrpc_c::carriageParm_wininet0 *
    operator->() const;
};

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransport_wininet : public xmlrpc_c::clientXmlTransport_http {

public:
    clientXmlTransport_wininet(bool const allowInvalidSslCerts = false);

    ~clientXmlTransport_wininet();
};

/*===========================================================================
                           pstream
===========================================================================*/

class XMLRPC_CLIENTPP_EXPORTED packetSocket;

class XMLRPC_CLIENTPP_EXPORTED carriageParm_pstream : public xmlrpc_c::carriageParm {

    // There are no parameters for carrying an RPC on a packet stream.
    // There's only one way to carry it.
};

class XMLRPC_CLIENTPP_EXPORTED carriageParm_pstreamPtr : public xmlrpc_c::carriageParmPtr {

public:
    carriageParm_pstreamPtr();
    carriageParm_pstreamPtr(
        xmlrpc_c::carriageParm_pstream * const carriageParmP);

    xmlrpc_c::carriageParm_pstream *
    operator->() const;
};

class XMLRPC_CLIENTPP_EXPORTED clientXmlTransport_pstream : public xmlrpc_c::clientXmlTransport {

public:
    struct constrOpt_impl;

    class XMLRPC_CLIENTPP_EXPORTED constrOpt {
    public:
        constrOpt();
        ~constrOpt();
        constrOpt(constrOpt&);

        constrOpt & fd                (int         const& arg);

    private:
        struct constrOpt_impl * implP;
        friend class clientXmlTransport_pstream;
    };

    clientXmlTransport_pstream(constrOpt const& opt);

    ~clientXmlTransport_pstream();

    void
    call(xmlrpc_c::carriageParm * const  carriageParmP,
         std::string              const& callXml,
         std::string *            const  responseXmlP);

private:
    packetSocket * packetSocketP;
};


} // namespace
#endif
