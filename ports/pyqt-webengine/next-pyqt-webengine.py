#!/usr/bin/env python3

import logging
import sys
from urllib.parse import urlparse

from PyQt5 import QtDBus
from PyQt5.QtNetwork import QNetworkProxy
# from PyQt5.QtNetwork import QNetworkProxyFactory
from PyQt5.QtWidgets import QApplication
from PyQt5.QtCore import pyqtSlot, Q_CLASSINFO, QObject

import buffers
import utility
import window

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


class DBusWindowAdaptor(QtDBus.QDBusAbstractAdaptor):
    Q_CLASSINFO("D-Bus Interface", PLATFORM_PORT_NAME)
    Q_CLASSINFO("D-Bus Introspection",
                """
<interface name="engineer.atlas.next.platform">
    <method name="window_make">
        <arg direction="in"  type="s" name="id" />
        <arg direction="out" type="s" name="new_id" />
    </method>
    <method name="window_set_title">
        <arg direction="in"  type="s" name="id" />
        <arg direction="in"  type="s" name="title" />
        <arg direction="out" type="b" name="status" />
    </method>
    <method name="window_delete">
        <arg direction="in"  type="s" name="id" />
        <arg direction="out" type="b" name="status" />
    </method>
    <method name="window_active">
        <arg direction="out" type="s" name="id" />
    </method>
    <method name="window_exists">
        <arg direction="in"  type="s" name="id" />
        <arg direction="out" type="b" name="status" />
    </method>
    <method name="window_set_active_buffer">
        <arg direction="in"  type="s" name="window_id" />
        <arg direction="in"  type="s" name="buffer_id" />
        <arg direction="out" type="b" name="status" />
    </method>
    <method name="window_set_minibuffer_height">
        <arg direction="in"  type="s" name="window_id" />
        <arg direction="in"  type="i" name="height" />
        <arg direction="out" type="x" name="new_height" />
    </method>
    <method name="buffer_make">
        <arg direction="in"  type="s" name="id" />
        <arg direction="in"  type="a{ss}" name="options" />
        <arg direction="out" type="s" name="new_id" />
    </method>
    <method name="buffer_delete">
        <arg direction="in"  type="s" name="id" />
        <arg direction="out" type="b" name="status" />
    </method>
    <method name="buffer_load">
        <arg direction="in"  type="s" name="buffer_id" />
        <arg direction="in"  type="s" name="url" />
        <arg direction="out" type="b" name="status" />
    </method>
    <method name="buffer_evaluate_javascript">
        <arg direction="in"  type="s" name="buffer_id" />
        <arg direction="in"  type="s" name="javascript" />
        <arg direction="out" type="s" name="callback_id"/>
    </method>
    <method name="minibuffer_evaluate_javascript">
        <arg direction="in"  type="s" name="window_id" />
        <arg direction="in"  type="s" name="javascript" />
        <arg direction="out" type="s" name="callback_id" />
    </method>
    <method name="set_proxy">
        <arg direction="in"  type="as" name="buffer_ids" />
        <arg direction="in"  type="s"  name="mode" />
        <arg direction="in"  type="s"  name="address" />
        <arg direction="in"  type="as" name="whitelist" />
        <arg direction="out" type="b"  name="status" />
    </method>
    <method name="get_proxy">
        <arg direction="out" type="s" />
    </method>
    <method name="generate_input_event">
        <arg direction="in"  type="s"  name="window_id" />
        <arg direction="in"  type="i"  name="key_code" />
        <arg direction="in"  type="as" name="modifiers" />
        <arg direction="in"  type="i"  name="low_level_data" />
        <arg direction="in"  type="d"  name="x" />
        <arg direction="in"  type="d"  name="y" />
        <arg direction="out" type="b"  name="status" />
    </method>
    <method name="window_killall">
    </method>
    <method name="window_list">
    </method>
    <method name="buffer_list">
    </method>
</interface>
                """)

    def __init__(self, parent):
        super(DBusWindowAdaptor, self).__init__(parent)
        self.setAutoRelaySignals(True)
        return

    @pyqtSlot(str, result=str)
    def window_make(self, window_id):
        return self.parent().window_make(window_id)

    @pyqtSlot(str, str, result=bool)
    def window_set_title(self, window_id, title):
        self.parent().window_set_title(window_id, title)
        return True

    @pyqtSlot(str, result=bool)
    def window_delete(self, window_id):
        return self.parent().window_delete(window_id)

    @pyqtSlot(result=str)
    def window_active(self):
        return self.parent().window_active()

    @pyqtSlot(str, result=bool)
    def window_exists(self, window_id):
        return self.parent().window_exists(window_id)

    @pyqtSlot(str, str, result=bool)
    def window_set_active_buffer(self, window_id, buffer_id):
        return self.parent().window_set_active_buffer(window_id, buffer_id)

    @pyqtSlot(str, int, result=int)
    def window_set_minibuffer_height(self, window_id, height):
        return self.parent().window_set_minibuffer_height(window_id, height)

    # It's easier to pass structs over dbus using QDBusMessage, than to specify
    # args + types in the decorator. See
    # https://www.riverbankcomputing.com/static/Docs/PyQt5/dbus.html
    @pyqtSlot(QtDBus.QDBusMessage, result=str)
    def buffer_make(self, msg):
        args = msg.arguments()
        buffer_id = args[0]
        options = args[1]

        return self.parent().buffer_make(buffer_id, options)

    @pyqtSlot(str, result=bool)
    def buffer_delete(self, buffer_id):
        return self.parent().buffer_delete(buffer_id)

    @pyqtSlot(str, str, result=bool)
    def buffer_load(self, buffer_id, url):
        return self.parent().buffer_load(buffer_id, url)

    @pyqtSlot(str, str, result=str)
    def buffer_evaluate_javascript(self, buffer_id, script):
        return self.parent().buffer_evaluate_javascript(buffer_id, script)

    @pyqtSlot(str, str, result=str)
    def minibuffer_evaluate_javascript(self, window_id, script):
        return self.parent().minibuffer_evaluate_javascript(window_id, script)

    @pyqtSlot('QStringList', str, str, 'QStringList', result=bool)
    def set_proxy(self, buffer_ids, mode, address, whitelist):
        return self.parent().set_proxy(buffer_ids, mode, address, whitelist)

    @pyqtSlot(result=str)
    def get_proxy(self):
        return self.parent().get_proxy()

    @pyqtSlot(str, int, 'QStringList', int, float, float)
    def generate_input_event(self, window_id, key_code, modifiers,
                             low_level_data, x, y):
        return self.parent().generate_input_event(window_id, key_code,
                                                  modifiers, low_level_data,
                                                  x, y)

    @pyqtSlot()
    def window_killall(self):
        return self.parent().window_killall()

    @pyqtSlot()
    def window_list(self):
        return self.parent().window_list()

    @pyqtSlot()
    def buffer_list(self):
        return self.parent().buffer_list()


class DBusWindow(QObject):
    def __init__(self):
        super().__init__()
        self.adaptor = DBusWindowAdaptor(self)
        return

    def window_make(self, window_id):
        return window.make(window_id)

    def window_set_title(self, window_id, title):
        _window = window.get_window(window_id)
        return _window.set_title(title)

    def window_delete(self, window_id):
        _window = window.get_window(window_id)
        return _window.delete()

    def window_active(self):
        return window.active()

    def window_exists(self, window_id):
        try:
            _window = window.get_window(window_id)
            return _window.exists()
        except Exception:
            return False

    def window_set_active_buffer(self, window_id, buffer_id):
        _window = window.get_window(window_id)
        _buffer = buffers.get_buffer(buffer_id)

        if _window and _buffer:
            return _window.set_active_buffer(_buffer)

        return False

    def window_set_minibuffer_height(self, window_id, height):
        _window = window.get_window(window_id)
        return _window.set_minibuffer_height(height)

    def buffer_make(self, buffer_id, options):
        return buffers.make(buffer_id, options)

    def buffer_delete(self, buffer_id):
        return buffers.delete(buffer_id)

    def buffer_load(self, buffer_id, url):
        _buffer = buffers.get_buffer(buffer_id)
        return _buffer.load(url)

    def buffer_evaluate_javascript(self, buffer_id, script):
        _buffer = buffers.get_buffer(buffer_id)
        return _buffer.evaluate_javascript(script)

    def minibuffer_evaluate_javascript(self, window_id, script):
        _window = window.get_window(window_id)
        return _window.minibuffer_evaluate_javascript(script)

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

    def get_proxy(self):
        QNetworkProxy.hostName(QNetworkProxy.applicationProxy())

    def generate_input_event(self, window_id, key_code, modifiers,
                             low_level_data, x, y):
        utility.generate_input_event(window_id, key_code, modifiers,
                                     low_level_data, x, y)

    #  DEVELOPER HELP FUNCTION
    def window_killall(self):
        return utility.killall()

    #  DEVELOPER HELP FUNCTION
    def window_list(self):
        return utility.list_windows()

    #  DEVELOPER HELP FUNCTION
    def buffer_list(self):
        return utility.list_buffers()


def main(app):
    app.setApplicationName("Next")

    dbus_window = DBusWindow()
    session_bus = QtDBus.QDBusConnection.sessionBus()
    session_bus.registerObject(PLATFORM_PORT_OBJECT_PATH, dbus_window)
    session_bus.registerService(PLATFORM_PORT_NAME)

    event_filter = utility.EventFilter(app)  # noqa: F841
    logging.info("Listening...")

    return app.exec()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    sys.exit(main(app))
