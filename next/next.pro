QT          += widgets printsupport uitools
TEMPLATE    = app
ICON        = ../assets/next.icns
CONFIG      += no_keywords release
INCLUDEPATH += /usr/local/include
INCLUDEPATH += /usr/local/include/eql
LIBS        += -lecl -L. -lnext -L/usr/local/lib -leql5
TARGET      = next
DESTDIR     = ./
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

SOURCES += main.cpp
