#!/usr/bin/env python3

import logging
import sys
from urllib.parse import urlparse

import dbus
import dbus.service
from PyQt5.QtNetwork import QNetworkProxy
# from PyQt5.QtNetwork import QNetworkProxyFactory
from PyQt5.QtWidgets import QApplication

import buffers
import utility
import window

try:
    from dbus.mainloop.pyqt5 import DBusQtMainLoop as MainLoop
except ImportError:
    from dbus.mainloop.glib import DBusGMainLoop as MainLoop

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
    # Lisp core dbus proxy.
    core_dbus_proxy = None

    def __init__(self, conn, object_path=PLATFORM_PORT_OBJECT_PATH, core_dbus_proxy=None):
        dbus.service.Object.__init__(self, conn, object_path)
        self.core_dbus_proxy = core_dbus_proxy

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='s')
    def window_make(self, window_id):
        return window.make(window_id)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='ss')
    def window_set_title(self, window_id, title):
        _window = window.get_window(window_id)
        return _window.set_title(title)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='s')
    def window_delete(self, window_id):
        _window = window.get_window(window_id)
        return _window.delete()

    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_active(self):
        return window.active()

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='s')
    def window_exists(self, window_id):
        try:
            _window = window.get_window(window_id)
            return _window.exists()
        except Exception:
            return False

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='ss')
    def window_set_active_buffer(self, window_id, buffer_id):
        _window = window.get_window(window_id)
        _buffer = buffers.get_buffer(buffer_id)
        return _window.set_active_buffer(_buffer)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='si')
    def window_set_minibuffer_height(self, window_id, height):
        _window = window.get_window(window_id)
        return _window.set_minibuffer_height(height)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='s')
    def buffer_make(self, buffer_id):
        return buffers.make(buffer_id)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='s')
    def buffer_delete(self, buffer_id):
        _buffer = buffers.get_buffer(buffer_id)
        return _buffer.delete()

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='ss')
    def buffer_load(self, buffer_id, url):
        _buffer = buffers.get_buffer(buffer_id)
        return _buffer.load(url)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='ss', out_signature='s')
    def buffer_evaluate_javascript(self, buffer_id, script):
        _buffer = buffers.get_buffer(buffer_id)
        return _buffer.evaluate_javascript(script)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='ss')
    def minibuffer_evaluate_javascript(self, window_id, script):
        _window = window.get_window(window_id)
        return _window.minibuffer_evaluate_javascript(script)

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='asssas', out_signature='b')
    def set_proxy(self, buffer_ids, mode, address, whitelist):
        """
        Set the proxy for the current application.

        TODO: set it for the given buffers.
        TODO: handle whitelist.

        https://doc.qt.io/qt-5.9/qtwebengine-overview.html#proxy-support
        """
        # XXX: currently the address in proxy-mode is 'socks5://127.0.0.1:9050'.
        # After changes there (and Gtk side) we won't do this url mangling.
        parsed_url = urlparse(address)
        scheme = parsed_url.scheme.lower()
        if 'socks5' in scheme:
            proxy_type = QNetworkProxy.Socks5Proxy
        elif 'httpproxy' in scheme:
            proxy_type = QNetworkProxy.HttpProxy
        elif 'httpcaching' in scheme:
            proxy_type = QNetworkProxy.HttpCachingProxy
        elif 'default' in scheme:
            proxy_type = QNetworkProxy.DefaultProxy
            logging.warn("Using the default proxy is currently unimplemented.")

        if ':' in parsed_url.netloc:
            address, port = parsed_url.netloc.split(':')
            try:
                port = int(port)
            except Exception:
                logging.warn("Invalid port: '{}'. Could not parse it as an int.".formatport)
                return False

        proxy = QNetworkProxy()
        proxy.setType(proxy_type)
        proxy.setHostName(address)
        proxy.setPort(port)

        QNetworkProxy.setApplicationProxy(proxy)
        logging.info("proxy set to address '{}'.".format(address))

        # Trying to use system's defaults:
        # QNetworkProxyFactory.setUseSystemConfiguration(True)

    @dbus.service.method(PLATFORM_PORT_NAME, out_signature='s')
    def get_proxy(self):
        QNetworkProxy.hostName(QNetworkProxy.applicationProxy())

    @dbus.service.method(PLATFORM_PORT_NAME, in_signature='siasidd')
    def generate_input_event(self, window_id, key_code, modifiers, low_level_data, x, y):
        utility.generate_input_event(window_id, key_code, modifiers, low_level_data, x, y)

    #  DEVELOPER HELP FUNCTION
    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_killall(self):
        return utility.killall()

    #  DEVELOPER HELP FUNCTION
    @dbus.service.method(PLATFORM_PORT_NAME)
    def window_list(self):
        return utility.list_windows()

    #  DEVELOPER HELP FUNCTION
    @dbus.service.method(PLATFORM_PORT_NAME)
    def buffer_list(self):
        return utility.list_buffers()


def main():
    app = QApplication(sys.argv)
    app.setApplicationName("Next")
    MainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()
    # name/dbuswindow MUST be defined even if not used.
    name = dbus.service.BusName('engineer.atlas.next.platform', session_bus)  # noqa: F841
    dbuswindow = DBusWindow(session_bus)  # noqa: F841
    event_filter = utility.EventFilter(app)  # noqa: F841
    logging.info("Listening...")
    sys.exit(app.exec_())


if __name__ == '__main__':
    main()
