#ifndef XMLRPC_C_C_UTIL_H_INCLUDED
#define XMLRPC_C_C_UTIL_H_INCLUDED

/* C language stuff.  Doesn't involve any libraries that aren't part of
   the compiler.
*/

/* XMLRPC_PRINTF_ATTR lets the GNU compiler check printf-type
   calls to be sure the arguments match the format string, thus preventing
   runtime segmentation faults and incorrect messages.
*/
#ifdef __GNUC__
#define XMLRPC_PRINTF_ATTR(a,b) __attribute__ ((format (printf, a, b)))
#define XMLRPC_NORETURN_ATTR __attribute__((noreturn))
#else
#define XMLRPC_PRINTF_ATTR(a,b)
#define XMLRPC_NORETURN_ATTR
#endif

/* XMLRPC_DLLEXPORT is an attribute of an external symbol that says it
   is to be exported from a library that contains it.

   XMLRPC_BUILD_DLL says the compilation at hand is for use in an Xmlrpc-c
   DLL.  This is meant to be defined via compiler option.
*/
#if defined(XMLRPC_BUILD_DLL) && defined(_MSC_VER)
#define XMLRPC_DLLEXPORT __declspec(dllexport)
#else
#define XMLRPC_DLLEXPORT
#endif

#endif
