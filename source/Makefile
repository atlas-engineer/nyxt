PKG_MODULES := gobject-2.0 webkit2gtk-web-extension-4.0 json-glib-1.0

CFLAGS += ${shell pkg-config ${PKG_MODULES} --cflags}
LDLIBS += ${shell pkg-config ${PKG_MODULES} --libs}

.PHONY: all install clean
all: libnyxt.so

libnyxt.so: nyxt.o globals.o
	${LD} ${LDFLAGS} -fPIC -shared -o $@ $^ ${LDLIBS}

nyxt.o: nyxt.c
	${CC} -fPIC -o $@ -c ${CFLAGS} $^

globals.o: globals.c
	${CC} -fPIC -o $@ -c ${CFLAGS} $^

install: libnyxt.so
	${INSTALL} $^

clean:
	${RM} libnyxt.so nyxt.o
