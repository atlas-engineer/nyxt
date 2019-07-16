/*
Copyright © 2018-2019 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <gtk/gtk.h>
#include <stdlib.h>
#include "server.h"

static void on_name_acquired(GDBusConnection *connection,
	const gchar *name,
	gpointer user_data) {
	g_message("Starting platform port");
}

static void on_name_lost(GDBusConnection *_connection,
	const gchar *_name,
	gpointer _user_data) {
	g_message("Platform port disconnected");
	// TODO: Call stop_server here?
	exit(1);
}

int main(int argc, char *argv[]) {
	// It's safer to initialize GTK before the server is started.  In particular,
	// without it the program would crash if hardware acceleration is disabled
	// (e.g. with the WEBKIT_DISABLE_COMPOSITING_MODE=1 environment variable).
	gtk_init(NULL, NULL);
	// TODO: Use GtkApplication?

	guint owner_id = g_bus_own_name(G_BUS_TYPE_SESSION,
			PLATFORM_PORT_NAME,
			G_BUS_NAME_OWNER_FLAGS_NONE,
			start_server,
			on_name_acquired,
			on_name_lost,
			NULL,
			NULL);

	// TODO: Start the RPC server first?  If GUI is started, then we can
	// report RPC startup issues graphically.

	gtk_main();

	stop_server();

	g_bus_unown_name(owner_id);
	return EXIT_SUCCESS;
}
