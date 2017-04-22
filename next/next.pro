QT          += widgets printsupport uitools
TEMPLATE     = app
CONFIG      += no_keywords release
LIBS        += -lecl -leql5 -L/usr/local/lib -lmy_lib -L.
TARGET       = next
DESTDIR      = ./
OBJECTS_DIR  = ./tmp/
MOC_DIR      = ./tmp/

win32 {
    include(../src/windows.pri)
}

SOURCES += main.cpp

INCLUDEPATH += /usr/local/include/
INCLUDEPATH += /usr/local/include/eql
