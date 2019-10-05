import logging
import utility

from PyQt5.QtCore import QUrl
from PyQt5.QtWebEngineWidgets import QWebEngineProfile, QWebEngineView

import core_interface

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
        # assign interceptor to avoid garbage collection
        self.interceptor = utility.WebEngineUrlRequestInterceptor(self.identifier)
        profile.setUrlRequestInterceptor(self.interceptor)
        profile.setPersistentCookiesPolicy(QWebEngineProfile.AllowPersistentCookies)

        # listen for page loading
        self.urlChanged.connect(self.did_commit_navigation)
        self.loadFinished.connect(self.did_finish_navigation)

        page.setHtml("")  # necessary for runJavaScript to work on new buffers.

        # Queue JS calls from the core so that we can hold them until the page
        # is ready to display/run them.
        self.js_ready = False
        self.script_queue = []

        # Consider ourselves ready to run JS when page has finished loading.
        self.js_ready_signal = page.loadFinished
        self.js_ready_signal.connect(self.set_js_ready)

        self.js_wait_signal = page.urlChanged
        self.js_wait_signal.connect(self.set_js_wait)

        self.js_ready_signal.connect(self.run_js_queue)

    def set_js_ready(self):
        self.js_ready = True

    def set_js_wait(self):
        self.js_ready = False

    def did_commit_navigation(self, url):
        """Invoked whenever the webview starts navigation.
        """
        core_interface.buffer_did_commit_navigation(self.identifier, str(url.url()))

    def did_finish_navigation(self, status):
        """Invoked whenever the webview finishes navigation.

        :param status: From documentation: Will indicate whether the
        load was successful or an error occurred.
        """
        url = self.url().url()
        core_interface.buffer_did_finish_navigation(self.identifier, str(url))

    def run_js_queue(self, ok):
        """Run ALL queued JavaScript calls."""
        for script in self.script_queue:
            self.page().runJavaScript(
                script,
                lambda x: self.javascript_callback(x, str(self.callback_count))
            )

        self.script_queue = []

    def evaluate_javascript(self, script):
        """
        This method returns an identifier (str) to the LISP core. Upon
        completion of the javascript script, the platform port will
        make a call to the LISP core with the results of that
        computation, and the associated identifier.

        Return: a callback_id (str).
        """

        self.callback_count += 1
        self.script_queue.append(script)

        if self.js_ready:  # If we're ready to rock and roll, then GO!
            self.run_js_queue(True)

        return str(self.callback_count)

    def javascript_callback(self, res, callback_id):
        if res is None:
            return
        core_interface.buffer_javascript_call_back(self.identifier, str(res),
                                                   callback_id)

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
