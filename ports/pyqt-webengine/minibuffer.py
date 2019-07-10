import logging

import core_interface

from PyQt5.QtWebEngineWidgets import QWebEngineView


class MiniBuffer():
    """Documentation for MiniBuffer

    """
    view = None
    scripts = {}
    callback_count = 0

    def __init__(self):
        super(MiniBuffer, self).__init__()
        self.view = QWebEngineView()

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

    def javascript_callback(self, res, callback_id):
        logging.debug("JS result is: {}".format(res))
        if res is None:
            return
        core_interface.buffer_javascript_call_back(str(self.identifier), res, callback_id)

    def set_height(self, height):
        self.view.setFixedHeight(height)

    def set_html(self, html):
        self.view.setHtml(html)
