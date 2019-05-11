/*
Copyright © 2018-2019 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

// This is kept outside of server.h because window.h also needs it.

#ifndef NEXT_PLATFORM_PORT
#define NEXT_PLATFORM_PORT 8082
#endif
#ifndef NEXT_CORE_SOCKET
#define NEXT_CORE_SOCKET "http://localhost:8081/RPC2"
#endif

typedef struct {
	gint port;
	gchar *core_socket;
	GHashTable *windows;
	GHashTable *buffers;
	GHashTable *server_callbacks;
	gchar *auth;
} ServerState;

static ServerState state = {
	.port = NEXT_PLATFORM_PORT,
	.core_socket = NEXT_CORE_SOCKET,
};
