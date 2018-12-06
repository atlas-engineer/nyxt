#ifndef CLIENT_SIMPLE_HPP_INCLUDED
#define CLIENT_SIMPLE_HPP_INCLUDED

#include <string>

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/base.hpp>
#include <xmlrpc-c/client.hpp>

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

class XMLRPC_CLIENTPP_EXPORTED clientSimple {

public:
    clientSimple();

    void
    call(std::string       const serverUrl,
         std::string       const methodName,
         xmlrpc_c::value * const resultP);

    void
    call(std::string       const serverUrl,
         std::string       const methodName,
         std::string       const format,
         xmlrpc_c::value * const resultP,
         ...);

    void
    call(std::string         const  serverUrl,
         std::string         const  methodName,
         xmlrpc_c::paramList const& paramList,
         xmlrpc_c::value *   const  resultP);

private:
    xmlrpc_c::clientPtr clientP;
};

} // namespace
#endif




