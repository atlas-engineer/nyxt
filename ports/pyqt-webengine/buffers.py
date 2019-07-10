import logging

import core_interface

from PyQt5.QtCore import QUrl
from PyQt5.QtWebEngineWidgets import QWebEngineView

#: A dictionary of current buffers mapping an identifier (str) to a
#  buffer (Buffer).
BUFFERS = {}


def get_buffer(identifier):
    buffer = BUFFERS.get(identifier)
    if buffer:
        return buffer
    else:
        raise Exception("Buffer ID: {} not found!".format(identifier))


class Buffer():
    """
    Documentation for Buffer.
    """
    view = None
    scripts = {}
    identifier = "0"
    callback_count = 0

    def __init__(self, identifier=None):
        super(Buffer, self).__init__()
        self.view = QWebEngineView()
        self.identifier = identifier

    def evaluate_javascript(self, script):
        """
        This method returns an identifier (str) to the LISP core. Upon
        completion of the javascript script, the platform port will
        make a call to the LISP core with the results of that
        computation, and the associated identifier.

        Return: a callback_id (str).
        """
        self.callback_count += 1
        self.view.page().runJavaScript(
            script,
            lambda x: self.javascript_callback(x, str(self.callback_count)))
        return str(self.callback_count)

    def javascript_callback(self, res, callback_id):
        logging.debug("JS result is: {}".format(res))
        if res is None:
            return
        core_interface.buffer_javascript_call_back(str(self.identifier), res, callback_id)

    def set_height(self, height):
        self.view.setFixedHeight(height)

    def load(self, url):
        self.view.setUrl(QUrl(url))
        return True

    def delete(self):
        self.view.hide()
        return True


def make(identifier: str):
    """Create a buffer, assign it the given unique identifier (str).

    We must pass a reference to the lisp core's dbus proxy, in order
    to send asynchronous input events (key, mouse etc).

    return: The Buffer identifier
    """
    assert isinstance(identifier, str)
    buffer = Buffer(identifier=identifier)
    BUFFERS[buffer.identifier] = buffer
    logging.info("New buffer created, id {}".format(buffer.identifier))
    return identifier
