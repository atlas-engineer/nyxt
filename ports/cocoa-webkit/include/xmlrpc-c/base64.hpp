#ifndef XMLRPC_BASE64_HPP_INCLUDED
#define XMLRPC_BASE64_HPP_INCLUDED

#include <string>
#include <vector>

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

namespace xmlrpc_c {


enum newlineCtl {NEWLINE_NO, NEWLINE_YES};

XMLRPC_LIBPP_EXPORTED
std::string
base64FromBytes(
    std::vector<unsigned char> const& bytes,
    xmlrpc_c::newlineCtl       const  newlineCtl = xmlrpc_c::NEWLINE_YES);


XMLRPC_LIBPP_EXPORTED
std::vector<unsigned char>
bytesFromBase64(std::string const& base64);


} // namespace

#endif
