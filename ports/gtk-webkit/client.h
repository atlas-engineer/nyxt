/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/
#pragma once

#include <libsoup/soup.h>

static SoupSession *xmlrpc_env;

void start_client() {
	xmlrpc_env = soup_session_new_with_options("timeout", 5);
}
