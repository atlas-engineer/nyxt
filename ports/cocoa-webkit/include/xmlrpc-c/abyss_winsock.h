/* This is just a sub-file for abyss.h */

/* See restrictions on including <winsock.h> and <windows.h> at the top
   of abyss.h.
*/
#include <winsock2.h>
#include <ws2tcpip.h>

struct abyss_win_chaninfo {
    size_t peerAddrLen;
    struct sockaddr peerAddr;
};

XMLRPC_ABYSS_EXPORTED
void
ChanSwitchWinCreate2(int                     const protocolFamily,
                      const struct sockaddr * const sockAddrP,
                      socklen_t               const sockAddrLen,
                      TChanSwitch **          const chanSwitchPP,
                      const char **           const errorP);

XMLRPC_ABYSS_EXPORTED
void
ChanSwitchWinCreate(unsigned short const portNumber,
                    TChanSwitch ** const chanSwitchPP,
                    const char **  const errorP);

XMLRPC_ABYSS_EXPORTED
void
ChanSwitchWinCreateWinsock(SOCKET         const winsock,
                           TChanSwitch ** const chanSwitchPP,
                           const char **  const errorP);

XMLRPC_ABYSS_EXPORTED
void
ChannelWinCreateWinsock(SOCKET                       const fd,
                        TChannel **                  const channelPP,
                        struct abyss_win_chaninfo ** const channelInfoPP,
                        const char **                const errorP);

typedef SOCKET TOsSocket;
