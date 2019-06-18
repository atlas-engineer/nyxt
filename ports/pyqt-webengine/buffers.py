from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtCore import QUrl
import logging

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
    """Documentation for Buffer

    """
    view = None
    scripts = {}

    def __init__(self):
        super(Buffer, self).__init__()
        self.view = QWebEngineView()
        self.set_url("http://next.atlas.engineer/")

    def evaluate_javascript(self, script):
        # This method should return an identifier to the LISP
        # core. Upon completion of the javascript script, the platform
        # port will make a call to the LISP core with the results of
        # that computation, and the associated identifier.
        self.view.page().runJavaScript(script)

    def load(self, url):
        self.view.setUrl(QUrl(url))
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
