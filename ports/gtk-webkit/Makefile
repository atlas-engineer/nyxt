## GNU Makefile.

CFLAGS += `pkg-config --cflags webkit2gtk-4.0`
LDLIBS += `pkg-config --libs webkit2gtk-4.0`

CFLAGS += `pkg-config --cflags xmlrpc_server_abyss`
LDLIBS += `pkg-config --libs xmlrpc_server_abyss`

all: next-gtk-webkit

.PHONY: debug
debug:
	CFLAGS+="-g3 -O0 -DDEBUG=9 -Wall -Wextra -pedantic -Wstrict-overflow -fno-strict-aliasing" ${MAKE}
	# CFLAGS+="-g3 -O0 -DDEBUG=9 -Wall -Wextra -pedantic -Wstrict-overflow -fno-strict-aliasing -Werror -Wshadow" ${MAKE}
