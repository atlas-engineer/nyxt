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
# from window import window_make
# from window import window_delete
# from window import window_send_event
# from window import set_title

"""
This is a Next port with Qt's Web Engine, through PyQt.

It is possible to test this from the Python or the Lisp REPL.

To send commands to the web engine from Lisp:
- start the PyQt port (make run)
- start lisp, quickload next
- create an interface and start it:

Now you can use any built-in methods such as (window-make myinterface) or send
custom signals with

    (send-signal myinterface "set_minibuffer" "yiha from CL!")

which prints its return value (an html snippet) and which should
change your minibuffer prompt.
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


def client_run(client, cmd, *args):
    pass


def set_minibuffer(name):
    # TODO: do in thread.
    """
    Change the minibuffer prompt and return its current html.
    """
    mb_prompt = """
    <html>
    <div> hello minibuffer </div>
    </html>
    """
    html = mb_prompt.replace("minibuffer", name)
    # wrapper = partial(minibuffer.setHtml, html)
    # QTimer.singleShot(0, wrapper)
    return html


if __name__ == '__main__':
    app = QApplication([])
    # DBusQtMainLoop(set_as_default=True)
    DBusGMainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()
    name = dbus.service.BusName('engineer.atlas.next.platform', session_bus)
    object = DBusWindow(session_bus)
    window.window_make("0")
    app.exec_()
