from dbus.mainloop.pyqt5 import DBusQtMainLoop
# from dbus.mainloop.glib import DBusGMainLoop
import dbus
import dbus.service

from PyQt5.QtWidgets import QApplication

import window

"""
This is a Next port with Qt's Web Engine, through PyQt.

It is possible to test this from the Python or the Lisp REPL.

To send commands to the web engine from Lisp:
- start the PyQt port (make run)
- start lisp, quickload next
- start and initialize the lisp side:

    (in-package :next)
    (start)

this creates an `*interface*` object.
Now you can use any built-in methods such as (window-make *interface*).
"""

PLATFORM_PORT_OBJECT_PATH = "/engineer/atlas/next/platform"
PLATFORM_PORT_NAME = "engineer.atlas.next.platform"


class DBusWindow(dbus.service.Object):
    def __init__(self, conn, object_path=PLATFORM_PORT_OBJECT_PATH):
        dbus.service.Object.__init__(self, conn, object_path)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_make(self, window_id):
        print("--- make_window")
        return ("s", window_id)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_set_title(self, window_id, title):
        return window.set_title(window_id, title)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_delete(self, window_id):
        return window.window_delete(window_id)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_active(self):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_exists(self, window_id):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_set_active_buffer(self, window_id, buffer_id):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_set_minibuffer_height(self, window_id, height):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def buffer_make(self, buffer_id):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def buffer_delete(self, buffer_id):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def buffer_load(self, buffer_id, url):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def buffer_evaluate_javascript(self, buffer_id, javascript):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def minibuffer_evaluate_javascript(self, window_id, javascript):
        pass

    @dbus.service.method(PLATFORM_PORT_NAME)
    def generate_input_event(self):
        pass


if __name__ == '__main__':
    app = QApplication([])
    DBusQtMainLoop(set_as_default=True)
    # DBusGMainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()
    name = dbus.service.BusName('engineer.atlas.next.platform', session_bus)
    dbuswindow = DBusWindow(session_bus)
    window.window_make("0")
    app.exec_()
