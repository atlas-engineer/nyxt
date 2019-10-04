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

        # Keep track of whether the page is finished loading
        self.ready = False

    def did_commit_navigation(self, url):
        """Invoked whenever the webview starts navigation.
        """
        core_interface.buffer_did_commit_navigation(self.identifier, str(url.url()))
        self.ready = False

    def did_finish_navigation(self, status):
        """Invoked whenever the webview finishes navigation.

        :param status: From documentation: Will indicate whether the
        load was successful or an error occurred.
        """
        url = self.url().url()
        core_interface.buffer_did_finish_navigation(self.identifier, str(url))
        self.ready = True

    def evaluate_javascript(self, script):
        """
        This method returns an identifier (str) to the LISP core. Upon
        completion of the javascript script, the platform port will
        make a call to the LISP core with the results of that
        computation, and the associated identifier.

        Return: a callback_id (str).
        """

        self.callback_count += 1

        # FIXME: Attaching this to a slot results in a memory leak!!!
        def run_javascript_handler(ok):
            return self.page().runJavaScript(
                script,
                lambda x: self.javascript_callback(x, str(self.callback_count)))

        # If called in the middle of a page-load, then wait till the load is
        # complete to run the javascript.
        if not self.ready:
            self.page().loadFinished.connect(run_javascript_handler)
        else:  # Otherwise, run it NOW!
            run_javascript_handler(True)

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
