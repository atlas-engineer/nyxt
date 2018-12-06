#ifndef CLIENT_HPP_INCLUDED
#define CLIENT_HPP_INCLUDED

#include <string>
#include <vector>
#include <memory>

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/girerr.hpp>
#include <xmlrpc-c/girmem.hpp>
#include <xmlrpc-c/base.hpp>
#include <xmlrpc-c/timeout.hpp>
#include <xmlrpc-c/client_transport.hpp>

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

namespace xmlrpc_c {

class XMLRPC_CLIENTPP_EXPORTED clientTransactionPtr;

class XMLRPC_CLIENTPP_EXPORTED clientTransaction : public girmem::autoObject {

    friend class clientTransactionPtr;

public:
    virtual void
    finish(xmlrpc_c::rpcOutcome const& outcome) = 0;
    
    virtual void
    finishErr(girerr::error const& error) = 0;

    virtual void
    progress(struct xmlrpc_progress_data const& progressData) const = 0;

protected:
    clientTransaction();
};

class XMLRPC_CLIENTPP_EXPORTED clientTransactionPtr : public girmem::autoObjectPtr {
    
public:
    clientTransactionPtr();

    clientTransactionPtr(clientTransaction * const transP);

    virtual ~clientTransactionPtr();

    virtual xmlrpc_c::clientTransaction *
    operator->() const;
};

class XMLRPC_CLIENTPP_EXPORTED clientPtr;

class XMLRPC_CLIENTPP_EXPORTED client : public girmem::autoObject {
/*----------------------------------------------------------------------------
   A generic client -- a means of performing an RPC.  This is so generic
   that it can be used for clients that are not XML-RPC.

   This is a base class.  Derived classes define things such as that
   XML and HTTP get used to perform the RPC.
-----------------------------------------------------------------------------*/
    friend class clientTransactionPtr;

public:
    virtual ~client();

    virtual void
    call(xmlrpc_c::carriageParm * const  carriageParmP,
         std::string              const& methodName,
         xmlrpc_c::paramList      const& paramList,
         xmlrpc_c::rpcOutcome *   const  outcomeP) = 0;

    virtual void
    start(xmlrpc_c::carriageParm *       const  carriageParmP,
          std::string                    const& methodName,
          xmlrpc_c::paramList            const& paramList,
          xmlrpc_c::clientTransactionPtr const& tranP);

    void
    finishAsync(xmlrpc_c::timeout const timeout);
    
    virtual void
    setInterrupt(int *);
};

class XMLRPC_CLIENTPP_EXPORTED clientPtr : public girmem::autoObjectPtr {
public:
    clientPtr();

    explicit clientPtr(xmlrpc_c::client * const clientP);

    xmlrpc_c::client *
    operator->() const;

    xmlrpc_c::client *
    get() const;
};

class XMLRPC_CLIENTPP_EXPORTED serverAccessor : public girmem::autoObject {
    
public:
    serverAccessor(xmlrpc_c::clientPtr       const clientP,
                   xmlrpc_c::carriageParmPtr const carriageParmP);

    void
    call(std::string            const& methodName,
         xmlrpc_c::paramList    const& paramList,
         xmlrpc_c::rpcOutcome * const  outcomeP) const;

private:
    xmlrpc_c::clientPtr       const clientP;
    xmlrpc_c::carriageParmPtr const carriageParmP;
};

class XMLRPC_CLIENTPP_EXPORTED serverAccessorPtr : public girmem::autoObjectPtr {
public:
    serverAccessorPtr();

    explicit
    serverAccessorPtr(xmlrpc_c::serverAccessor * const serverAccessorP);

    xmlrpc_c::serverAccessor *
    operator->() const;

    xmlrpc_c::serverAccessor *
    get() const;
};

class XMLRPC_CLIENTPP_EXPORTED connection {
/*----------------------------------------------------------------------------
   A nexus of a particular client and a particular server, along with
   carriage parameters for performing RPCs between the two.

   This is a minor convenience for client programs that always talk to
   the same server the same way.

   Use this as a parameter to rpc.call().
-----------------------------------------------------------------------------*/
public:
    connection(xmlrpc_c::client *       const clientP,
               xmlrpc_c::carriageParm * const carriageParmP);

    ~connection();

    xmlrpc_c::client *       clientP;
    xmlrpc_c::carriageParm * carriageParmP;
};

class XMLRPC_CLIENTPP_EXPORTED client_xml : public xmlrpc_c::client {
/*----------------------------------------------------------------------------
   A client that uses XML-RPC XML in the RPC.  This class does not define
   how the XML gets transported, though (i.e. does not require HTTP).
-----------------------------------------------------------------------------*/
public:
    client_xml(xmlrpc_c::clientXmlTransport * const transportP);

    client_xml(xmlrpc_c::clientXmlTransport * const transportP,
               xmlrpc_dialect                 const dialect);

    client_xml(xmlrpc_c::clientXmlTransportPtr const transportP);

    client_xml(xmlrpc_c::clientXmlTransportPtr const transportP,
               xmlrpc_dialect                  const dialect);

    ~client_xml();

    void
    call(carriageParm *         const  carriageParmP,
         std::string            const& methodName,
         xmlrpc_c::paramList    const& paramList,
         xmlrpc_c::rpcOutcome * const  outcomeP);

    void
    start(xmlrpc_c::carriageParm *       const  carriageParmP,
          std::string                    const& methodName,
          xmlrpc_c::paramList            const& paramList,
          xmlrpc_c::clientTransactionPtr const& tranP);

    void
    finishAsync(xmlrpc_c::timeout const timeout);

    virtual void
    setInterrupt(int * interruptP);

private:
    struct client_xml_impl * implP;
};

class XMLRPC_CLIENTPP_EXPORTED xmlTransaction_client : public xmlrpc_c::xmlTransaction {

public:
    xmlTransaction_client(xmlrpc_c::clientTransactionPtr const& tranP);

    void
    finish(std::string const& responseXml) const;

    void
    finishErr(girerr::error const& error) const;

    void
    progress(xmlrpc_progress_data const& progressData) const;

private:
    xmlrpc_c::clientTransactionPtr const tranP;
};

class XMLRPC_CLIENTPP_EXPORTED xmlTransaction_clientPtr : public xmlTransactionPtr {
public:
    xmlTransaction_clientPtr();
    
    xmlTransaction_clientPtr(xmlrpc_c::clientTransactionPtr const& tranP);

    xmlrpc_c::xmlTransaction_client *
    operator->() const;
};

class rpcPtr;

class XMLRPC_CLIENTPP_EXPORTED rpc : public clientTransaction {
/*----------------------------------------------------------------------------
   An RPC.  An RPC consists of method name, parameters, and result.  It
   does not specify in any way how the method name and parameters get
   turned into a result.  It does not presume XML or HTTP.
  
   You don't normally create or reference an object of this class directly,
   but rather via an 'rpcPtr' object.  That takes care of deleting the object
   when you are done with it (but not before).  This is critical if you plan
   to use the 'start' method, because without an rpcPtr reference, the system
   will destroy the object under the covers when the RPC finishes, and there
   is no way for you to guarantee you won't still access it after it finishes
   (because of accesses within Xmlrpc-c calls such as the call that finishes
   the RPC or just rpc::start).
 
   In order to do asynchronous RPCs, you normally have to create a derived
   class that defines a useful notifyComplete().
-----------------------------------------------------------------------------*/
    friend class xmlrpc_c::rpcPtr;

public:
    void
    call(xmlrpc_c::client       * const clientP,
         xmlrpc_c::carriageParm * const carriageParmP);

    void
    call(xmlrpc_c::connection const& connection);

    void
    start(xmlrpc_c::client       * const clientP,
          xmlrpc_c::carriageParm * const carriageParmP);
    
    void
    start(xmlrpc_c::connection const& connection);
    
    void
    finish(xmlrpc_c::rpcOutcome const& outcome);

    void
    finishErr(girerr::error const& error);

    virtual void
    notifyComplete();

    virtual void
    progress(struct xmlrpc_progress_data const& progressData) const;

    bool
    isFinished() const;

    bool
    isSuccessful() const;

    xmlrpc_c::value
    getResult() const;

    xmlrpc_c::fault
    getFault() const;

    rpc(std::string         const  methodName,
        xmlrpc_c::paramList const& paramList);

    virtual ~rpc();

private:
    struct rpc_impl * implP;
};

class XMLRPC_CLIENTPP_EXPORTED rpcPtr : public clientTransactionPtr {
public:
    rpcPtr();

    explicit rpcPtr(xmlrpc_c::rpc * const rpcP);

    rpcPtr(std::string         const  methodName,
           xmlrpc_c::paramList const& paramList);

    xmlrpc_c::rpc *
    operator->() const;
};

} // namespace
#endif
