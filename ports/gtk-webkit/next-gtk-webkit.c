/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <gtk/gtk.h>
#include <stdlib.h>
#include "server.h"

int main(int argc, char *argv[]) {
	// TODO: Use GtkApplication?
	GError *error = NULL;
	char *default_port = g_strdup_printf("%i", NEXT_PLATFORM_PORT);
	GOptionEntry options[] = {
		{"port", 'p', 0, G_OPTION_ARG_INT, &state.port, "Port the XML-RPC server listens to", default_port},
		{"core-socket", 's', 0, G_OPTION_ARG_STRING, &state.core_socket, "Socket of the Lisp core", NEXT_CORE_SOCKET},
		{NULL}
	};

	gtk_init_with_args(&argc, &argv, "", options, NULL, &error);
	if (error) {
		g_error("%s", error->message);
		g_error_free(error);
		g_free(default_port);
		return EXIT_FAILURE;
	}
	g_free(default_port);

	// TODO: Start the xmlrpc server first?  If GUI is started, then we can
	// report xmlrpc startup issue graphically.
	start_server();
	start_client();

	gtk_main();

	stop_server();
	return EXIT_SUCCESS;
}
