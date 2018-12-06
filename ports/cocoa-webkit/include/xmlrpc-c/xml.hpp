#ifndef XML_HPP_INCLUDED
#define XML_HPP_INCLUDED

#include <string>
#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/base.hpp>

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

namespace xmlrpc_c {
namespace xml {

XMLRPC_LIBPP_EXPORTED
void
generateCall(std::string         const& methodName,
             xmlrpc_c::paramList const& paramList,
             std::string *       const  callXmlP);
    
XMLRPC_LIBPP_EXPORTED
void
generateCall(std::string         const& methodName,
             xmlrpc_c::paramList const& paramList,
             xmlrpc_dialect      const  dialect,
             std::string *       const  callXmlP);
    
XMLRPC_LIBPP_EXPORTED
void
parseCall(std::string           const& callXml,
          std::string *         const  methodNameP,
          xmlrpc_c::paramList * const  paramListP);

XMLRPC_LIBPP_EXPORTED
void
generateResponse(xmlrpc_c::rpcOutcome const& outcome,
                 xmlrpc_dialect       const  dialect,
                 std::string *        const  respXmlP);

XMLRPC_LIBPP_EXPORTED
void
generateResponse(xmlrpc_c::rpcOutcome const& outcome,
                 std::string *        const  respXmlP);

XMLRPC_LIBPP_EXPORTED
void
parseSuccessfulResponse(std::string       const& responseXml,
                        xmlrpc_c::value * const  resultP);

XMLRPC_LIBPP_EXPORTED
void
parseResponse(std::string            const& responseXml,
              xmlrpc_c::rpcOutcome * const  outcomeP);

XMLRPC_LIBPP_EXPORTED
void
trace(std::string const& label,
      std::string const& xml);


}} // namespace
#endif
