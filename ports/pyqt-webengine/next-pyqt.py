from functools import partial
# from dbus.mainloop.pyqt5 import DBusQtMainLoop
from dbus.mainloop.glib import DBusGMainLoop
import dbus
import dbus.service

from PyQt5.QtCore import QThread
from PyQt5.QtCore import pyqtSignal
from PyQt5.QtWidgets import QApplication
from PyQt5.QtWidgets import QWidget

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
    def ping(self):
        return 'pong'

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_make(self, uid):
        print("--- make_window")
        return window.window_make(uid)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_set_title(self, uid, title):
        return window.set_title(uid, title)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_delete(self, uid):
        return window.window_delete(uid)

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_killall(self):
        return window.window_killall()

    @dbus.service.method(PLATFORM_PORT_NAME)
    def set_minibuffer(self, identifier, text):
        """
        Change the minibuffer of the given window with `text` and return its current html.
        """
        prompt = """
        <html>
        <div> hello minibuffer </div>
        </html>
        """
        html = prompt.replace("minibuffer", text)
        window.set_minibuffer(identifier, html)
        return html

    @dbus.service.method(PLATFORM_PORT_NAME)
    def set_minibuffer_height(self, identifier, height):
        window.set_minibuffer_height(identifier, height)


if __name__ == '__main__':
    app = QApplication([])
    # DBusQtMainLoop(set_as_default=True)
    DBusGMainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()
    name = dbus.service.BusName('engineer.atlas.next.platform', session_bus)
    dbuswindow = DBusWindow(session_bus)
    window.window_make("0")
    app.exec_()
