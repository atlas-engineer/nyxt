/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

// This is kept outside of server.h because window.h also needs it.

#include "autokey-dictionary.h"

typedef struct {
	AutokeyDictionary *windows;
	AutokeyDictionary *buffers;
	GHashTable *server_callbacks;
} ServerState;

static ServerState state;
