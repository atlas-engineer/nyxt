#ifndef XMLRPC_C_CONFIG_H_INCLUDED
#define XMLRPC_C_CONFIG_H_INCLUDED

/* This file, part of XML-RPC For C/C++, is meant to 
   define characteristics of this particular installation 
   that the other <xmlrpc-c/...> header files need in 
   order to compile correctly when #included in Xmlrpc-c
   user code.

   Those header files #include this one.

   This file was created by a make rule.
*/
#define XMLRPC_HAVE_WCHAR 1
#ifdef _WIN32
  /* SOCKET is a type defined by <winsock.h>.  Anyone who
     uses XMLRPC_SOCKET on a WIN32 system must #include
     <winsock.h>
  */
  #define XMLRPC_SOCKET SOCKET
  #define XMLRPC_HAVE_TIMEVAL 0
  #define XMLRPC_HAVE_TIMESPEC 0
  #define XMLRPC_HAVE_PTHREAD 0
  #define XMLRPC_HAVE_WINTHREAD 1
#else
  #define XMLRPC_SOCKET int
  #define XMLRPC_HAVE_TIMEVAL 1
  #define XMLRPC_HAVE_TIMESPEC 1
  #define XMLRPC_HAVE_PTHREAD 1
  #define XMLRPC_HAVE_WINTHREAD 0
#endif

#if defined(_MSC_VER)
  /* Newer MSVC has long long, but MSVC 6 does not */
  #define XMLRPC_INT64 __int64
  #define XMLRPC_PRId64 "I64"
  #define XMLRPC_INT32 __int32
#else
  #define XMLRPC_INT64 long long
  #define XMLRPC_PRId64 "lld"
  #define XMLRPC_INT32 int
#endif
#endif
