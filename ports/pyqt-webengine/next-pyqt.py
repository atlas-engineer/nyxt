import logging

import utility
import window

# from dbus.mainloop.glib import DBusGMainLoop
import dbus
import dbus.service
from dbus.mainloop.pyqt5 import DBusQtMainLoop
from PyQt5.QtWidgets import QApplication

logging.basicConfig(level=logging.INFO)

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

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='s')
    def window_make(self, window_id):
        return window.window_make(window_id)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_set_title(self, window_id, title):
        return window.set_title(window_id, title)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_delete(self, window_id):
        return window.window_delete(window_id)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_killall(self):
        return window.window_killall()

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
        window.set_minibuffer_height(window_id, height)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_set_minibuffer(self, window_id, text):
        window.set_minibuffer(window_id, text)

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
        utility.generate_input_event()


def main():
    app = QApplication([])
    DBusQtMainLoop(set_as_default=True)
    # DBusGMainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()
    # name/dbuswindow MUST be defined even if not used.
    name = dbus.service.BusName('engineer.atlas.next.platform', session_bus)  # noqa: F841
    dbuswindow = DBusWindow(session_bus)  # noqa: F841
    window.window_make("0")
    app.exec_()


if __name__ == '__main__':
    main()
