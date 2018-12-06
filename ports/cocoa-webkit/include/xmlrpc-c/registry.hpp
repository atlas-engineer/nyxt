#ifndef REGISTRY_HPP_INCLUDED
#define REGISTRY_HPP_INCLUDED

#include <sys/types.h>
#include <string>
#include <vector>
#include <list>

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/girmem.hpp>
#include <xmlrpc-c/base.hpp>

namespace xmlrpc_c {


/*
  XMLRPC_SERVERPP_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_server++.

  XMLRPC_BUILDING_SERVERPP says this compilation is part of
  libxmlrpc_server++, as opposed to something that _uses_
  libxmlrpc_server++.
*/
#ifdef XMLRPC_BUILDING_SERVERPP
#define XMLRPC_SERVERPP_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_SERVERPP_EXPORTED
#endif

class XMLRPC_SERVERPP_EXPORTED callInfo {
/*----------------------------------------------------------------------------
   Information about how an XML-RPC call arrived.

   This base class carries no information; Servers that don't have any
   call information to provide might use this.  Servers that do have call
   information to provide define a derived class of this that contains
   information pertinent to that kind of server.
-----------------------------------------------------------------------------*/
public:
    virtual ~callInfo();  // This makes it polymorphic

    callInfo();
};

class XMLRPC_SERVERPP_EXPORTED method : public girmem::autoObject {
/*----------------------------------------------------------------------------
   An XML-RPC method.

   This base class is abstract.  You can't create an object in it.
   Define a useful method with this as a base class, with an
   execute() method.
-----------------------------------------------------------------------------*/
public:
    method();

    virtual ~method();

    virtual void
    execute(xmlrpc_c::paramList const& paramList,
            xmlrpc_c::value *   const  resultP) = 0;

    std::string signature() const { return _signature; };
    std::string help() const { return _help; };

protected:
    std::string _signature;
    std::string _help;
};

/* Example of a specific method class:

   class sample_add : public xmlrpc_c::method {
   public:
       sample_add() {
           this->_signature = "ii";
           this->_help = "This method adds two integers together";
       }
       void
       execute(xmlrpc_c::param_list    const paramList,
               const xmlrpc_c::value * const retvalP) {
          
           int const addend(paramList.getInt(0));
           int const adder(paramList.getInt(1));

           *retvalP = xmlrpc_c::value(addend, adder);
      }
   };


   Example of creating such a method:

   methodPtr const sampleAddMethodP(new sample_add);

   You pass around, copy, etc. the handle sampleAddMethodP and when
   the last copy of the handle is gone, the sample_add object itself
   gets deleted.

*/


class XMLRPC_SERVERPP_EXPORTED method2 : public method {
/*----------------------------------------------------------------------------
   An XML-RPC method.

   This base class is abstract.  You can't create an object in it.
   Define a useful method with this as a base class, with an
   execute() method.

   This differs from class 'method' in that the execute() method gets
   call information ('callInfo').
-----------------------------------------------------------------------------*/
public:
    method2();

    virtual ~method2();

    virtual void
    execute(xmlrpc_c::paramList        const& paramList,
            const xmlrpc_c::callInfo * const  callInfoP,
            xmlrpc_c::value *          const  resultP) = 0;

    void
    execute(xmlrpc_c::paramList const& paramList,
            xmlrpc_c::value *   const  resultP);

};

class XMLRPC_SERVERPP_EXPORTED methodPtr : public girmem::autoObjectPtr {

public:
    methodPtr(xmlrpc_c::method * const methodP);

    xmlrpc_c::method *
    operator->() const;
};

class XMLRPC_SERVERPP_EXPORTED defaultMethod : public girmem::autoObject {

public:
    virtual ~defaultMethod();

    virtual void
    execute(std::string         const& methodName,
            xmlrpc_c::paramList const& paramList,
            xmlrpc_c::value *   const  resultP) = 0;
};

class XMLRPC_SERVERPP_EXPORTED defaultMethodPtr : public girmem::autoObjectPtr {

public:
    defaultMethodPtr();

    defaultMethodPtr(xmlrpc_c::defaultMethod * const methodP);

    xmlrpc_c::defaultMethod *
    operator->() const;

    xmlrpc_c::defaultMethod *
    get() const;
};

struct registry_impl;

class XMLRPC_SERVERPP_EXPORTED registry : public girmem::autoObject {
/*----------------------------------------------------------------------------
   An Xmlrpc-c server method registry.  An Xmlrpc-c server transport
   (e.g.  an HTTP server) uses this object to process an incoming
   Xmlrpc-c call.
-----------------------------------------------------------------------------*/

public:

    registry();
    ~registry();

    void
    addMethod(std::string        const name,
              xmlrpc_c::method * const methodP);

    void
    addMethod(std::string         const name,
              xmlrpc_c::methodPtr const methodP);

    void
    setDefaultMethod(xmlrpc_c::defaultMethod * const methodP);

    void
    setDefaultMethod(xmlrpc_c::defaultMethodPtr const methodP);

    void
    disableIntrospection();

    class XMLRPC_SERVERPP_EXPORTED shutdown {
    public:
        virtual ~shutdown() = 0;
        virtual void
        doit(std::string const& comment,
             void *      const  callInfo) const = 0;
    };

    void
    setShutdown(const shutdown * const shutdownP);

    void
    setDialect(xmlrpc_dialect const dialect);
    
    void
    processCall(std::string   const& callXml,
                std::string * const  responseXmlP) const;

    void
    processCall(std::string                const& callXml,
                const xmlrpc_c::callInfo * const callInfoP,
                std::string *              const  responseXmlP) const;
        
    size_t
    maxStackSize() const;

private:

    registry_impl * implP;
};


class XMLRPC_SERVERPP_EXPORTED registryPtr : public girmem::autoObjectPtr {

public:
    registryPtr();

    registryPtr(xmlrpc_c::registry * const registryP);

    xmlrpc_c::registry *
    operator->() const;

    xmlrpc_c::registry *
    get() const;
};

} // namespace

#endif
