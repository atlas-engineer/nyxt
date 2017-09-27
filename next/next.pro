QT          += widgets printsupport uitools
TEMPLATE    = app
ICON        = ../assets/next.icns
CONFIG      += no_keywords release
INCLUDEPATH += /usr/local/include
INCLUDEPATH += /usr/local/include/eql
LIBS        += -L/usr/local/lib -lecl
LIBS        += -L/usr/local/lib -leql5
LIBS        += -L. -lnext
TARGET      = next
DESTDIR     = ./
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

SOURCES += main.cpp

