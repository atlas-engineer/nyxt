from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtCore import QUrl


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

    def set_url(self, url):
        self.view.setUrl(QUrl(url))
