#ifndef GIRERR_HPP_INCLUDED
#define GIRERR_HPP_INCLUDED

#include <string>
#include <exception>

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

#define HAVE_GIRERR_ERROR

namespace girerr {

class XMLRPC_LIBPP_EXPORTED error : public std::exception {
public:
    error(std::string const& what_arg) : _what(what_arg) {}

    ~error() throw() {}

    virtual const char *
    what() const throw() { return this->_what.c_str(); };

private:
    std::string _what;
};

// throwf() always throws a girerr::error .

XMLRPC_LIBPP_EXPORTED
void
throwf(const char * const format, ...)
  XMLRPC_PRINTF_ATTR(1,2)
  XMLRPC_NORETURN_ATTR;

} // namespace

#endif
