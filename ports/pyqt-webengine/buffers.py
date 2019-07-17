import logging

import core_interface

from PyQt5.QtCore import QUrl
from PyQt5.QtWebEngineWidgets import QWebEngineProfile, QWebEngineView

#: A dictionary of current buffers mapping an identifier (str) to a
#  buffer (Buffer).
BUFFERS = {}


def get_buffer(identifier):
    buffer = BUFFERS.get(identifier)
    if buffer:
        return buffer
    else:
        raise Exception("Buffer ID: {} not found!".format(identifier))


class Buffer(QWebEngineView):
    """
    Documentation for Buffer.
    """
    scripts = {}
    identifier = "0"
    callback_count = 0

    def __init__(self, identifier=None, parent=None):
        super(Buffer, self).__init__(parent)
        self.identifier = str(identifier)
        page = self.page()
        profile = page.profile()
        # listen for page loading
        self.loadStarted.connect(self.did_commit_navigation)
        profile.setPersistentCookiesPolicy(QWebEngineProfile.AllowPersistentCookies)

    def did_commit_navigation(self):
        core_interface.buffer_did_commit_navigation(self.identifier, str(self.url))

    def did_finish_navigation(self):
        logging.info("Finish Navigation")

    def evaluate_javascript(self, script):
        """
        This method returns an identifier (str) to the LISP core. Upon
        completion of the javascript script, the platform port will
        make a call to the LISP core with the results of that
        computation, and the associated identifier.

        Return: a callback_id (str).
        """
        self.callback_count += 1
        self.page().runJavaScript(
            script,
            lambda x: self.javascript_callback(x, str(self.callback_count)))
        return str(self.callback_count)

    def javascript_callback(self, res, callback_id):
        if res is None:
            return
        core_interface.buffer_javascript_call_back(self.identifier, res, callback_id)

    def set_height(self, height):
        self.setFixedHeight(height)

    def load(self, url):
        self.setUrl(QUrl(url))
        return True

    def delete(self):
        self.hide()
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
