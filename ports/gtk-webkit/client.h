/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <libsoup/soup.h>

static SoupSession *xmlrpc_env;

void start_client() {
	xmlrpc_env = soup_session_new();
}
