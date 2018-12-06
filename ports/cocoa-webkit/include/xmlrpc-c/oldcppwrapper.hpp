// -*- C++ -*-   <-- an Emacs control

// Copyright information is at the bottom of the file.

//=========================================================================
//  XML-RPC C++ API
//=========================================================================


#ifndef XMLRPCCPP_H_INCLUDED
#define XMLRPCCPP_H_INCLUDED

/*
  XMLRPC_OLDCPPWRAPPER_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_cpp.

  XMLRPC_BUILDING_OLDCPPWRAPPER says this compilation is part of
  libxmlrpc_cpp, as opposed to something that _uses_ libxmlrpc_cpp.
*/
#ifdef XMLRPC_BUILDING_OLDCPPWRAPPER
#define XMLRPC_OLDCPPWRAPPER_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_OLDCPPWRAPPER_EXPORTED
#endif

#include <cstdlib>
#include <string>

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>

//=========================================================================
//  XmlRpcFault
//=========================================================================
//  A C++ exception class representing an XML-RPC fault.

class XMLRPC_OLDCPPWRAPPER_EXPORTED XmlRpcFault {

private:
    xmlrpc_env   mFault;

    XmlRpcFault& operator= (XmlRpcFault const& f)
        { if (true || f.getFaultCode()) abort(); return (XmlRpcFault&) f; }

public:
    XmlRpcFault (const XmlRpcFault &fault);
    XmlRpcFault (const int faultCode, const std::string faultString);
    XmlRpcFault (const xmlrpc_env *env);
    ~XmlRpcFault (void);

    inline int getFaultCode (void) const;
    std::string getFaultString (void) const;
    inline xmlrpc_env * getFaultEnv (void);
};

inline int XmlRpcFault::getFaultCode (void) const {
    return mFault.fault_code;
}

inline xmlrpc_env *XmlRpcFault::getFaultEnv (void) {
    return &mFault;
}


//=========================================================================
//  XmlRpcEnv
//=========================================================================
//  This class can be used to wrap xmlrpc_env object. Use it as follows:
//
//    XmlRpcEnv env;
//    xmlrpc_parse_value(env, v, "(i)", &i);
//    env.throwIfFaultOccurred();        

class XMLRPC_OLDCPPWRAPPER_EXPORTED XmlRpcEnv {

private:
    xmlrpc_env   mEnv;

    void         throwMe (void) const;
    XmlRpcEnv&   operator= (XmlRpcEnv const& e)
        { if (true || e.faultOccurred()) abort(); return (XmlRpcEnv&) e;}

public:
    XmlRpcEnv (const XmlRpcEnv &env);
    XmlRpcEnv (void) { xmlrpc_env_init(&mEnv); }
    ~XmlRpcEnv (void) { xmlrpc_env_clean(&mEnv); }
    
    bool     faultOccurred (void) const {return (mEnv.fault_occurred != 0);};
    bool         hasFaultOccurred (void) const { return faultOccurred(); };
        /* hasFaultOccurred() is for backward compatibility.
           faultOccurred() is a superior name for this.
        */
    std::string  getFaultString() const { return mEnv.fault_string; };
    XmlRpcFault  getFault (void) const;

    void         throwIfFaultOccurred (void) const;

    operator xmlrpc_env * (void) { return &mEnv; }
};

inline void XmlRpcEnv::throwIfFaultOccurred (void) const {
    if (faultOccurred())
        throwMe();
}


//=========================================================================
//  XmlRpcValue
//=========================================================================
//  An object in this class is an XML-RPC value.
//
//  We have a complex structure to allow C code mixed in with C++ code
//  which uses this class to refer to the same XML-RPC value object.
//  This is especially important because there aren't proper C++ facilities
//  for much of Xmlrpc-c; you have to use the C facilities even if you'd
//  rather use proper C++.
//
//  The XmlRpcValue object internally represents the value as an
//  xmlrpc_value.  It holds one reference to the xmlrpc_value.  Users
//  of XmlRpcValue never see that xmlrpc_value, but C code can.  the
//  C code might create the xmlrpc_value and then bind it to an XmlRpcValue,
//  or it might get the xmlrpc_value handle from the XmlRpcValue.  Finally,
//  C code can simply use the XmlRpcValue where an xmlrpc_value handle is
//  required and it gets converted automatically.
//
//  So reference counting for the xmlrpc_value is quite a nightmare.

class XMLRPC_OLDCPPWRAPPER_EXPORTED XmlRpcValue {

private:
    xmlrpc_value *mValue;

public:
    enum ReferenceBehavior {
        MAKE_REFERENCE,
        CONSUME_REFERENCE
    };

    typedef xmlrpc_int32 int32;
    
    XmlRpcValue (void);
    XmlRpcValue (xmlrpc_value *value,
                 ReferenceBehavior behavior = MAKE_REFERENCE);
    XmlRpcValue (const XmlRpcValue& value);
    ~XmlRpcValue (void);
    
    XmlRpcValue&  operator= (const XmlRpcValue& value);

    // Accessing the value's type (think of this as lightweight RTTI).
    xmlrpc_type getType(void) const;
    
    // We don't supply an automatic conversion operator--you need to say
    // whether you want to make or borrow this object's reference.
    // XXX - Is it really OK for these to be const?
    xmlrpc_value *makeReference (void) const;
    xmlrpc_value *borrowReference (void) const;

    // Some static "constructor" functions.
    static XmlRpcValue makeInt      (const XmlRpcValue::int32 i);
    static XmlRpcValue makeBool     (const bool b);
    static XmlRpcValue makeDouble   (const double d);
    static XmlRpcValue makeDateTime (const std::string& dateTime);
    static XmlRpcValue makeString   (const std::string& str);
    static XmlRpcValue makeString   (const char *const str);
    static XmlRpcValue makeString   (const char *const str, size_t len);
    static XmlRpcValue makeArray    (void);
    static XmlRpcValue makeStruct   (void);
    static XmlRpcValue makeBase64   (const unsigned char *const data,
                                     size_t len);
    /*
    // An interface to xmlrpc_build_value.
    static XmlRpcValue buildValue (const char *const format, ...);
    */

    // Some functions to get the underlying data.
    // These will throw an XmlRpcFault if the data is the wrong type.
    XmlRpcValue::int32 getInt   (void) const;
    bool         getBool        (void) const;
    double       getDouble      (void) const;
    std::string  getRawDateTime (void) const;
    std::string  getString      (void) const;
    XmlRpcValue  getArray       (void) const;
    XmlRpcValue  getStruct      (void) const;

    // This returns an internal pointer which will become invalid when
    // all references to the underlying value are destroyed.
    void         getBase64      (const unsigned char *& out_data,
                                 size_t& out_len) const;

    /*
    // An interface to xmlrpc_parse_value.
    void parseValue (const char *const format, ...);
    */

    // Array functions. These will throw an XmlRpcFault if the value
    // isn't an array.
    size_t       arraySize (void) const;
    void         arrayAppendItem (const XmlRpcValue& value);
    XmlRpcValue  arrayGetItem (int index) const;
    
    // Struct functions. These will throw an XmlRpcFault if the value
    // isn't a struct.
    size_t       structSize (void) const;
    bool         structHasKey (const std::string& key) const;
    XmlRpcValue  structGetValue (const std::string& key) const;
    void         structSetValue (const std::string& key, 
                                 const XmlRpcValue& value);
    void         structGetKeyAndValue (const int index,
                                       std::string& out_key,
                                       XmlRpcValue& out_value) const;
};

inline XmlRpcValue::XmlRpcValue (xmlrpc_value *value,
                                 ReferenceBehavior behavior) 
{
    mValue = value;

    if (behavior == MAKE_REFERENCE)
        xmlrpc_INCREF(value);
}

inline XmlRpcValue::XmlRpcValue (const XmlRpcValue& value) {
    mValue = value.mValue;
    xmlrpc_INCREF(mValue);
}

inline XmlRpcValue::~XmlRpcValue (void) {
    xmlrpc_DECREF(mValue);
}

inline XmlRpcValue& XmlRpcValue::operator= (const XmlRpcValue& value) {
    // Must increment before we decrement, in case of assignment to self.
    xmlrpc_INCREF(value.mValue);
    xmlrpc_DECREF(mValue);
    mValue = value.mValue;
    return *this;
}

inline xmlrpc_type XmlRpcValue::getType (void) const {
    return xmlrpc_value_type(mValue);
}

inline xmlrpc_value *XmlRpcValue::makeReference (void) const {
    xmlrpc_INCREF(mValue);
    return mValue;
}

inline xmlrpc_value *XmlRpcValue::borrowReference (void) const {
    return mValue;
}


//=========================================================================
//  XmlRpcClient
//=========================================================================

class XMLRPC_OLDCPPWRAPPER_EXPORTED XmlRpcClient {

private:
    std::string mServerUrl;

public:
    static void Initialize (std::string appname, std::string appversion);
    static void Terminate (void);

    XmlRpcClient (const std::string& server_url) : mServerUrl(server_url) {}
    ~XmlRpcClient (void) {}

    XmlRpcClient (const XmlRpcClient& client);
    XmlRpcClient& operator= (const XmlRpcClient& client);

    XmlRpcValue call (std::string method_name, XmlRpcValue param_array);
    void call_asynch (std::string method_name,
                      XmlRpcValue param_array,
                      xmlrpc_response_handler callback,
                      void* user_data);
    void event_loop_asynch (unsigned long milliseconds);
};

inline void XmlRpcClient::call_asynch(std::string method_name,
                                      XmlRpcValue param_array,
                                      xmlrpc_response_handler callback,
                                      void* user_data)
{
    xmlrpc_client_call_asynch_params(
        mServerUrl.c_str(),
        method_name.c_str(),
        callback,
        user_data,
        param_array.borrowReference());
}

inline void XmlRpcClient::event_loop_asynch(unsigned long milliseconds)
{
    xmlrpc_client_event_loop_finish_asynch_timeout(milliseconds);
}


//=========================================================================
//  XmlRpcClient Methods
//=========================================================================
//  These are inline for now, so we don't need to screw with linker issues
//  and build a separate client library.

inline XmlRpcClient::XmlRpcClient (const XmlRpcClient& client)
    : mServerUrl(client.mServerUrl)
{
}

inline XmlRpcClient& XmlRpcClient::operator= (const XmlRpcClient& client) {
    if (this != &client)
        mServerUrl = client.mServerUrl;
    return *this;
}

inline void XmlRpcClient::Initialize (std::string appname, 
                                      std::string appversion) {
    xmlrpc_client_init(XMLRPC_CLIENT_NO_FLAGS,
                       appname.c_str(),
                       appversion.c_str());
}

inline void XmlRpcClient::Terminate (void) {
    xmlrpc_client_cleanup();
}

inline XmlRpcValue XmlRpcClient::call (std::string method_name,
                                       XmlRpcValue param_array)
{
    XmlRpcEnv env;
    xmlrpc_value *result =
    xmlrpc_client_call_params(env,
                              mServerUrl.c_str(),
                              method_name.c_str(),
                              param_array.borrowReference());
    env.throwIfFaultOccurred();
    return XmlRpcValue(result, XmlRpcValue::CONSUME_REFERENCE);
}

//=========================================================================
//  XmlRpcGenSrv
//=========================================================================

class XMLRPC_OLDCPPWRAPPER_EXPORTED XmlRpcGenSrv {

private:

    xmlrpc_registry*    mRegistry;

    xmlrpc_mem_block* alloc (XmlRpcEnv& env, const std::string& body) const; 

public:

    XmlRpcGenSrv (int flags);
    ~XmlRpcGenSrv (void);

    xmlrpc_registry* getRegistry (void) const;

    XmlRpcGenSrv&   addMethod (const std::string& name,
                               xmlrpc_method method,
                               void *data);
    XmlRpcGenSrv&   addMethod (const std::string& name,
                               xmlrpc_method method,
                               void* data,
                               const std::string& signature,
                               const std::string& help);
    
    std::string handle (const std::string& body) const;
};

inline XmlRpcGenSrv::XmlRpcGenSrv (int) {

    XmlRpcEnv env;

    mRegistry = xmlrpc_registry_new (env);
    env.throwIfFaultOccurred();        
}

inline XmlRpcGenSrv::~XmlRpcGenSrv (void) {

    xmlrpc_registry_free (mRegistry);
}

inline xmlrpc_registry* XmlRpcGenSrv::getRegistry () const {

    return mRegistry;
}


// Copyright (C) 2001 by Eric Kidd. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//    derived from this software without specific prior written permission. 
//  
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.


#endif /* _XMLRPCCPP_H_ */
