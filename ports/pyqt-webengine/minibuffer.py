from PyQt5.QtWebEngineWidgets import QWebEngineView


class MiniBuffer():
    """Documentation for MiniBuffer

    """
    view = None
    scripts = {}

    def __init__(self):
        super(MiniBuffer, self).__init__()
        self.view = QWebEngineView()
        self.set_html("<style>body{padding:0;margin:0;}</style>Unset.")

    def evaluate_javascript(self, script):
        # This method should return an identifier to the LISP
        # core. Upon completion of the javascript script, the platform
        # port will make a call to the LISP core with the results of
        # that computation, and the associated identifier.
        self.view.page().runJavaScript(script)

    def set_height(self, height):
        self.view.setFixedHeight(height)

    def set_html(self, html):
        self.view.setHtml(html)
