## GNU Makefile.

## WebKitGTK depends on Glib, GTK, and libsoup.
CFLAGS += `pkg-config --cflags webkit2gtk-4.0`
LDLIBS += `pkg-config --libs webkit2gtk-4.0`

## Clang fails to parse command line if executable depends directly on headers.
next-gtk-webkit: next-gtk-webkit.o
next-gtk-webkit.o: $(wildcard *.h)

.PHONY: debug
debug:
	CFLAGS+="-g3 -O0 -DDEBUG=9 -Wall -Wextra -pedantic -Wstrict-overflow -fno-strict-aliasing -Wshadow" ${MAKE}
