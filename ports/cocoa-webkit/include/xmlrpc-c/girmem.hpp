/*============================================================================
                               girmem.hpp
==============================================================================
  This declares the user interface to memory management facilities (smart
  pointers, basically) in libxmlrpc.  They are used in interfaces to various
  classes in XML For C/C++.
============================================================================*/
#ifndef GIRMEM_HPP_INCLUDED
#define GIRMEM_HPP_INCLUDED

#include <memory>
#include <xmlrpc-c/c_util.h>

/*
  XMLRPC_LIBPP_EXPORTED marks a symbol in this file that is exported from
  libxmlrpc++.

  XMLRPC_BUILDING_LIBPP says this compilation is part of libxmlrpc++, as
  opposed to something that _uses_ libxmlrpc++.
*/
#ifdef XMLRPC_BUILDING_LIBPP
#define XMLRPC_LIBPP_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_LIBPP_EXPORTED
#endif

namespace girmem {

class XMLRPC_LIBPP_EXPORTED autoObjectPtr;

class XMLRPC_LIBPP_EXPORTED autoObject {
    friend class autoObjectPtr;

public:
    void incref();
    void decref(bool * const unreferencedP);

protected:
    autoObject();
    virtual ~autoObject();

private:
    class Impl;

    std::auto_ptr<Impl> const implP;

    // Because of 'implP', we cannot allow copy construction, so this is
    // private:
    autoObject(autoObject const&);
};

class XMLRPC_LIBPP_EXPORTED autoObjectPtr {
public:
    autoObjectPtr();
    autoObjectPtr(girmem::autoObject * objectP);
    autoObjectPtr(girmem::autoObjectPtr const& autoObjectPtr);
    
    ~autoObjectPtr();
    
    void
    point(girmem::autoObject * const objectP);

    void
    unpoint();

    autoObjectPtr
    operator=(girmem::autoObjectPtr const& objectPtr);
    
    girmem::autoObject *
    operator->() const;
    
    girmem::autoObject *
    get() const;

protected:
    girmem::autoObject * objectP;
};

} // namespace

#endif
