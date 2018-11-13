## GNU Makefile.

CFLAGS += `pkg-config --cflags webkit2gtk-4.0`
LDLIBS += `pkg-config --libs webkit2gtk-4.0`

CFLAGS += `pkg-config --cflags xmlrpc_server_abyss`
LDLIBS += `pkg-config --libs xmlrpc_server_abyss`

all: next-gtk-webkit

install: next-gtk-webkit
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f $< $(DESTDIR)$(PREFIX)/bin
	chmod 755 $(DESTDIR)$(PREFIX)/bin/$<

.PHONY: debug
debug:
	CFLAGS+="-g3 -O0 -DDEBUG=9 -Wall -Wextra -pedantic -Wstrict-overflow -fno-strict-aliasing -Wshadow" ${MAKE}
