#ifndef XMLRPC_TIMEOUT_H_INCLUDED
#define XMLRPC_TIMEOUT_H_INCLUDED

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

struct XMLRPC_LIBPP_EXPORTED timeout {

    timeout() : finite(false) {}

    timeout(unsigned int const duration) :
        finite(true), duration(duration) {}
        // 'duration' is the timeout time in milliseconds

    bool finite;
    unsigned int duration;  // in milliseconds
};


} // namespace

#endif
