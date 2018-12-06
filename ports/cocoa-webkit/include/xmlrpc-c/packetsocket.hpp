#ifndef PACKETSOCKET_HPP_INCLUDED
#define PACKETSOCKET_HPP_INCLUDED

/*============================================================================
                         packetsocket
==============================================================================
  This is a facility for communicating socket-style, with defined
  packets like a datagram socket but with reliable delivery like a
  stream socket.  It's like a POSIX "sequential packet" socket, except
  it is built on top of a stream socket, so it is usable on the many
  systems that have stream sockets but not sequential packet sockets.
============================================================================*/

#include <sys/types.h>
#include <string>
#include <queue>

#include <xmlrpc-c/c_util.h>
#include <xmlrpc-c/girmem.hpp>

/*
  XMLRPC_PACKETSOCKET_EXPORTED marks a symbol in this file that is exported
  from libxmlrpc_packetsocket.

  XMLRPC_BUILDING_PACKETSOCKET says this compilation is part of
  libxmlrpc_packetsocket, as opposed to something that _uses_
  libxmlrpc_packetsocket.
*/
#ifdef XMLRPC_BUILDING_PACKETSOCKET
#define XMLRPC_PACKETSOCKET_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_PACKETSOCKET_EXPORTED
#endif

namespace xmlrpc_c {

class XMLRPC_PACKETSOCKET_EXPORTED packet : public girmem::autoObject {

public:
    packet();

    packet(const unsigned char * const data,
           size_t                const dataLength);

    packet(const char * const data,
           size_t       const dataLength);

    ~packet();

    unsigned char *
    getBytes() const { return this->bytes; }

    size_t
    getLength() const { return this->length; }

    void
    addData(const unsigned char * const data,
            size_t                const dataLength);

private:
    unsigned char * bytes;  // malloc'ed
    size_t length;
    size_t allocSize;

    void
    initialize(const unsigned char * const data,
               size_t                const dataLength);
};



class XMLRPC_PACKETSOCKET_EXPORTED packetPtr: public girmem::autoObjectPtr {

public:
    packetPtr();

    explicit packetPtr(packet * const packetP);

    packet *
    operator->() const;
};



class XMLRPC_PACKETSOCKET_EXPORTED packetSocket_impl;

class XMLRPC_PACKETSOCKET_EXPORTED packetSocket {
/*----------------------------------------------------------------------------
   This is an Internet communication vehicle that transmits individual
   variable-length packets of text.

   It is based on a stream socket.

   It would be much better to use a kernel SOCK_SEQPACKET socket, but
   Linux 2.4 does not have them.
-----------------------------------------------------------------------------*/
public:
    packetSocket(int sockFd);

    ~packetSocket();

    void
    writeWait(packetPtr const& packetPtr) const;

    void
    read(bool *      const eofP,
         bool *      const gotPacketP,
         packetPtr * const packetPP);

    void
    readWait(volatile const int * const interruptP,
             bool *               const eofP,
             bool *               const gotPacketP,
             packetPtr *          const packetPP);

    void
    readWait(volatile const int * const interruptP,
             bool *               const eofP,
             packetPtr *          const packetPP);

    void
    readWait(bool *      const eofP,
             packetPtr * const packetPP);

private:
    packetSocket_impl * implP;
};



} // namespace

#endif
