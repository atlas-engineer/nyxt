#ifndef XMLRPC_BASE_HPP_INCLUDED
#define XMLRPC_BASE_HPP_INCLUDED

#include <xmlrpc-c/config.h>

#include <climits>
#include <cfloat>
#include <ctime>
#include <vector>
#include <map>
#include <string>
#if defined(__GNUC__) && __GNUC__ < 3
#include <iostream>
#else
#include <ostream>
#endif
#if XMLRPC_HAVE_TIMEVAL
#include <sys/time.h>
#endif

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/base.h>

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

class XMLRPC_LIBPP_EXPORTED value {
    // This is a handle.  You don't want to create a pointer to this;
    // it is in fact a pointer itself.
public:
    value();
        // This creates a placeholder.  It can't be used for anything, but
        // holds memory.  instantiate() can turn it into a real object.

    value(xmlrpc_c::value const &value);  // copy constructor

    ~value();

    enum type_t {
        // These are designed to be identical to the values for
        // enum xmlrpc_type in the C library.
        TYPE_INT        = 0,
        TYPE_BOOLEAN    = 1,
        TYPE_DOUBLE     = 2,
        TYPE_DATETIME   = 3,
        TYPE_STRING     = 4,
        TYPE_BYTESTRING = 5,
        TYPE_ARRAY      = 6,
        TYPE_STRUCT     = 7,
        TYPE_C_PTR      = 8,
        TYPE_NIL        = 9,
        TYPE_I8         = 10,
        TYPE_DEAD       = 0xDEAD
    };

    type_t type() const;

    xmlrpc_c::value&
    operator=(xmlrpc_c::value const&);

    bool
    isInstantiated() const;

    // The following are not meant to be public to users, but just to
    // other Xmlrpc-c library modules.  If we ever go to a pure C++
    // implementation, not based on C xmlrpc_value objects, this shouldn't
    // be necessary.

    void
    appendToCArray(xmlrpc_value * const arrayP) const;

    void
    addToCStruct(xmlrpc_value * const structP,
                 std::string    const key) const;

    xmlrpc_value *
    cValue() const;
        // Not to be confused with public 'cvalue' method that all the derived
        // classes have.

    value(xmlrpc_value * const valueP);

    void
    instantiate(xmlrpc_value * const valueP);
        // Works only on a placeholder object created by the no-argument
        // constructor.

    xmlrpc_value * cValueP;
        // NULL means this is merely a placeholder object.

protected:
    void
    validateInstantiated() const;
};


std::ostream& operator<<(std::ostream& out,
                         xmlrpc_c::value::type_t const& type);


class XMLRPC_LIBPP_EXPORTED value_int : public value {
public:
    value_int(int const cvalue);

    value_int(xmlrpc_c::value const baseValue);

    operator int() const;

    int cvalue() const;
};


class XMLRPC_LIBPP_EXPORTED value_boolean : public value {
public:
    value_boolean(bool const cvalue);

    value_boolean(xmlrpc_c::value const baseValue);

    operator bool() const;

    bool cvalue() const;
};


class XMLRPC_LIBPP_EXPORTED value_string : public value {
public:
    enum nlCode {nlCode_all, nlCode_lf};

    value_string(std::string const& cppvalue,
                 nlCode      const  nlCode);

    value_string(std::string const& cppvalue);

    value_string(xmlrpc_c::value const baseValue);

    std::string
    crlfValue() const;

    operator std::string() const;

    std::string cvalue() const;
};


class XMLRPC_LIBPP_EXPORTED value_double : public value {
public:
    value_double(double const cvalue);

    value_double(xmlrpc_c::value const baseValue);

    operator double() const;

    double cvalue() const;
};


class XMLRPC_LIBPP_EXPORTED value_datetime : public value {
public:
    value_datetime(std::string const cvalue);

    value_datetime(xmlrpc_datetime const cvalue);
    operator xmlrpc_datetime() const;

    value_datetime(time_t const cvalue);
    operator time_t() const;

#if XMLRPC_HAVE_TIMEVAL
    value_datetime(struct timeval const& cvalue);
    operator timeval() const;
#endif
#if XMLRPC_HAVE_TIMESPEC
    value_datetime(struct timespec const& cvalue);
    operator timespec() const;
#endif

    value_datetime(xmlrpc_c::value const baseValue);

    time_t cvalue() const;

    std::string iso8601Value() const;
};


typedef std::vector<unsigned char> cbytestring;

class XMLRPC_LIBPP_EXPORTED value_bytestring : public value {
public:
    value_bytestring(cbytestring const& cvalue);

    value_bytestring(xmlrpc_c::value const baseValue);

    // You can't cast to a vector because the compiler can't tell which
    // constructor to use (complains about ambiguity).  So we have this:
    cbytestring
    vectorUcharValue() const;

    cbytestring cvalue() const;

    size_t
    length() const;
};



typedef std::map<std::string, xmlrpc_c::value> cstruct;

class XMLRPC_LIBPP_EXPORTED value_struct : public value {
public:
    value_struct(cstruct const& cvalue);

    value_struct(xmlrpc_c::value const baseValue);

    operator cstruct() const;

    cstruct cvalue() const;
};



typedef std::vector<xmlrpc_c::value> carray;

class XMLRPC_LIBPP_EXPORTED value_array : public value {
public:
    value_array(carray const& cvalue);

    value_array(xmlrpc_c::value const baseValue);

    // You can't cast to a vector because the compiler can't tell which
    // constructor to use (complains about ambiguity).  So we have this:
    carray
    vectorValueValue() const;

    carray cvalue() const;

    size_t
    size() const;
};



class XMLRPC_LIBPP_EXPORTED value_nil : public value {
public:
    value_nil();

    value_nil(xmlrpc_c::value const baseValue);

    void * cvalue() const;
};


class XMLRPC_LIBPP_EXPORTED value_i8 : public value {
public:
    value_i8(xmlrpc_int64 const cvalue);

    value_i8(xmlrpc_c::value const baseValue);

    operator xmlrpc_int64() const;

    xmlrpc_int64 cvalue() const;
};


inline xmlrpc_c::value_string
toValue(const char * const x) {
    return xmlrpc_c::value_string(x);
}

inline xmlrpc_c::value_string
toValue(std::string const& x) {
    return xmlrpc_c::value_string(x);
}

inline xmlrpc_c::value_int
toValue(int const x) {
    return xmlrpc_c::value_int(x);
}

inline xmlrpc_c::value_boolean
toValue(bool const x) {
    return xmlrpc_c::value_boolean(x);
}

inline xmlrpc_c::value_double
toValue(double const x) {
    return xmlrpc_c::value_double(x);
}

inline xmlrpc_c::value_bytestring
toValue(cbytestring const& x) {
    return xmlrpc_c::value_bytestring(x);
}

inline const xmlrpc_c::value &
toValue(xmlrpc_c::value const& v) {
/*----------------------------------------------------------------------------
  This does a null conversion; you use it to catch all the XML-RPC types that
  have no usable C++ equivalent, so you can do a toValue() of any XML-RPC
  type at all.  In particular: 'value_datetime', 'value_nil'.
-----------------------------------------------------------------------------*/
    return v;
}

template<class K, class V> xmlrpc_c::value_struct
toValue(std::map<K, V> const& in) {
/*----------------------------------------------------------------------------
  convert C++ map to XML-RPC structure
-----------------------------------------------------------------------------*/
    cstruct ret;
    for (typename std::map<std::string, V>::const_iterator p = in.begin();
         p != in.end();
         ++p) {
        ret[p->first] = toValue(p->second);
    }
    return xmlrpc_c::value_struct(ret);
}

template<class InputIterator> xmlrpc_c::value_array
arrayValueSlice(InputIterator begin,
                InputIterator end) {
/*----------------------------------------------------------------------------
  convert C++ iterator pair to XML-RPC array
-----------------------------------------------------------------------------*/
    carray ret;
    for (InputIterator p = begin; p != end; ++p) {
        ret.push_back(toValue(*p));
    }
    return xmlrpc_c::value_array(ret);
}

template<class T> inline xmlrpc_c::value_array
toValue(std::vector<T> const& in) {
/*----------------------------------------------------------------------------
  convert C++ vector to XML-RPC array
-----------------------------------------------------------------------------*/
    return arrayValueSlice(in.begin(), in.end());
}

// fromValue() returns via reference argument instead of by return value
// so the compiler can tell which version of it to invoke based on the
// desired output type.

inline void
fromValue(std::string & y, xmlrpc_c::value const& x) {
    y = xmlrpc_c::value_string(x);
}

inline void
fromValue(int & y, xmlrpc_c::value const& x) {
    y = xmlrpc_c::value_int(x);
}

inline void
fromValue(bool & y, xmlrpc_c::value const& x) {
    y = xmlrpc_c::value_boolean(x);
}

inline void
fromValue(double & y, xmlrpc_c::value const& x) {
    y = xmlrpc_c::value_double(x);
}

inline void
fromValue(cbytestring & y, xmlrpc_c::value const& x) {
    y = xmlrpc_c::value_bytestring(x).vectorUcharValue();
}

inline void
fromValue(xmlrpc_c::value & y, xmlrpc_c::value const& x) {
/*----------------------------------------------------------------------------
  This does a null conversion; it's so you can use fromValue() with
  an XML-RPC value or C++ value without having to know which it is.
  One reason you would have an XML-RPC value lying around with C++ values
  is that some XML-RPC values don't have a common C++ equivalent.
-----------------------------------------------------------------------------*/
    y = x;
}

template<class K, class V> inline void
fromValue(std::map<K, V> & y, xmlrpc_c::value const& x) {
/*----------------------------------------------------------------------------
   Convert XML-RPC structure to C++ map.
-----------------------------------------------------------------------------*/
    cstruct m = xmlrpc_c::value_struct(x);
    y.clear();
    for (std::map<std::string, xmlrpc_c::value>::const_iterator p = m.begin();
         p != m.end();
         ++p) {
        fromValue(y[p->first], p->second);
    }
}

template<class T> inline void
fromValue(std::vector<T> & y, xmlrpc_c::value const& x) {
/*----------------------------------------------------------------------------
   Convert XML-RPC array to C++ vector.
-----------------------------------------------------------------------------*/
    carray v = xmlrpc_c::value_array(x).vectorValueValue();
    y.resize(v.size());
    for (unsigned int i = 0; i < v.size(); ++i) {
        fromValue(y[i], v[i]);
    }
}

template<class MemberClass> inline xmlrpc_c::value_array
arrayValueArray(const MemberClass * const in,
                size_t              const size) {
/*----------------------------------------------------------------------------
  convert C++ array to XML-RPC array
-----------------------------------------------------------------------------*/
    return arrayValueSlice(in, in + size);
}

class XMLRPC_LIBPP_EXPORTED fault {
/*----------------------------------------------------------------------------
   This is an XML-RPC fault.

   This object is not intended to be used to represent a fault in the
   execution of XML-RPC client/server software -- just a fault in an
   XML-RPC RPC as described by the XML-RPC spec.

   There is no way to represent "no fault" with this object.  The object is
   meaningful only in the context of some fault.
-----------------------------------------------------------------------------*/
public:
    enum code_t {
        CODE_UNSPECIFIED            =    0,
        CODE_INTERNAL               = -500,
        CODE_TYPE                   = -501,
        CODE_INDEX                  = -502,
        CODE_PARSE                  = -503,
        CODE_NETWORK                = -504,
        CODE_TIMEOUT                = -505,
        CODE_NO_SUCH_METHOD         = -506,
        CODE_REQUEST_REFUSED        = -507,
        CODE_INTROSPECTION_DISABLED = -508,
        CODE_LIMIT_EXCEEDED         = -509,
        CODE_INVALID_UTF8           = -510
    };

    fault();

    fault(std::string             const _faultString,
          xmlrpc_c::fault::code_t const _faultCode 
              = xmlrpc_c::fault::CODE_UNSPECIFIED
        );
    
    xmlrpc_c::fault::code_t getCode() const;

    std::string getDescription() const;

private:
    bool                    valid;
    xmlrpc_c::fault::code_t code;
    std::string             description;
};

class XMLRPC_LIBPP_EXPORTED rpcOutcome {
/*----------------------------------------------------------------------------
  The outcome of a validly executed RPC -- either an XML-RPC fault
  or an XML-RPC value of the result.
-----------------------------------------------------------------------------*/
public:
    rpcOutcome();
    rpcOutcome(xmlrpc_c::value const result);
    rpcOutcome(xmlrpc_c::fault const fault);
    bool succeeded() const;
    xmlrpc_c::fault getFault() const;
    xmlrpc_c::value getResult() const;
private:
    bool valid;
        // This is false in a placeholder variable -- i.e. an object you
        // create with the no-argument constructor, which is waiting to be
        // assigned a value.  When false, nothing below is valid.
    bool _succeeded;
    xmlrpc_c::value result;  // valid if 'succeeded'
    xmlrpc_c::fault fault;   // valid if not 'succeeded'
};

class XMLRPC_LIBPP_EXPORTED paramList {
/*----------------------------------------------------------------------------
   A parameter list of an XML-RPC call.
-----------------------------------------------------------------------------*/
public:
    paramList(unsigned int const paramCount = 0);

    paramList&
    add(xmlrpc_c::value const param);

    paramList&
    addx(xmlrpc_c::value const param);

    template<class T > paramList& addc(const T & x) {
        xmlrpc_c::paramList::add(toValue(x));
        return *this;
    }

    unsigned int
    size() const;

    xmlrpc_c::value operator[](unsigned int const subscript) const;

    int
    getInt(unsigned int const paramNumber,
           int          const minimum = INT_MIN,
           int          const maximum = INT_MAX) const;

    bool
    getBoolean(unsigned int const paramNumber) const;

    double
    getDouble(unsigned int const paramNumber,
              double       const minimum = -DBL_MAX,
              double       const maximum = DBL_MAX) const;

    enum timeConstraint {TC_ANY, TC_NO_PAST, TC_NO_FUTURE};

    time_t
    getDatetime_sec(unsigned int   const paramNumber,
                    timeConstraint const constraint
                        = paramList::TC_ANY) const;

    std::string
    getString(unsigned int const paramNumber) const;

    cbytestring
    getBytestring(unsigned int const paramNumber) const;

    carray
    getArray(unsigned int const paramNumber,
             unsigned int const minSize = 0,
             unsigned int const maxSize = UINT_MAX) const;

    cstruct
    getStruct(unsigned int const paramNumber) const;

    void
    getNil(unsigned int const paramNumber) const;

    xmlrpc_int64
    getI8(unsigned int const paramNumber,
          xmlrpc_int64 const minimum = XMLRPC_INT64_MIN,
          xmlrpc_int64 const maximum = XMLRPC_INT64_MAX) const;

    void
    verifyEnd(unsigned int const paramNumber) const;

private:
    std::vector<xmlrpc_c::value> paramVector;
};

} // namespace

#endif
