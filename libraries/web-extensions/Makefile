# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# REVIEW: Is there a better way to find the .pc file regardless of the version suffix?
PKG_MODULES := glib-2.0 gobject-2.0 $(shell pkg-config --list-all | awk '/^webkit2gtk-web-extension-4.1/ {print $$1}')

# See https://stackoverflow.com/questions/5618615/check-if-a-program-exists-from-a-makefile.
CC := $(shell command -v $(CC) 2> /dev/null || echo gcc)

CFLAGS += $(shell pkg-config $(PKG_MODULES) --cflags) -fPIC
LDLIBS += $(shell pkg-config $(PKG_MODULES) --libs)

objects = $(patsubst %.c,%.o,$(wildcard *.c))

.PHONY: all install clean
all: libnyxt.so

libnyxt.so: $(objects)
	$(CC) -fPIC -shared -o $@ $^ $(LDLIBS) $(LDFLAGS)

install: libnyxt.so
	$(INSTALL) $^

clean:
	$(RM) libnyxt.so $(objects)
