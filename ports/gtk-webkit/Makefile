## WebKitGTK depends on Glib, GTK, and libsoup.
CFLAGS += `pkg-config --cflags webkit2gtk-4.0`
LDLIBS += `pkg-config --libs webkit2gtk-4.0`

## Clang fails to parse command line if executable depends directly on headers.
next-gtk-webkit: next-gtk-webkit.o
next-gtk-webkit.o: *.h

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin

.PHONY: install
install: next-gtk-webkit
	mkdir -p "$(DESTDIR)$(BINDIR)"
	cp -f $< "$(DESTDIR)$(BINDIR)/"
	chmod 755 "$(DESTDIR)$(BINDIR)/"$<
