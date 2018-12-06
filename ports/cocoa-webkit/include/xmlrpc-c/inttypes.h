#ifndef XMLRPC_INTTYPES_H_INCLUDED
#define XMLRPC_INTTYPES_H_INCLUDED

#ifdef _MSC_VER

typedef unsigned short    xmlrpc_uint16_t;
typedef unsigned int      xmlrpc_uint32_t;
typedef unsigned __int64  xmlrpc_uint64_t;

#else
#include <inttypes.h>
#ifdef __INTERIX
#  include <stdint.h>
#endif

typedef uint16_t xmlrpc_uint16_t;
typedef uint32_t xmlrpc_uint32_t;
typedef uint64_t xmlrpc_uint64_t;

#endif

#endif
