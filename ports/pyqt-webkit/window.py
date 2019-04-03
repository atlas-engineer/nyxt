from functools import partial

from PyQt5.QtCore import QTimer
from PyQt5.QtCore import QUrl
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWidgets import QVBoxLayout
from PyQt5.QtWidgets import QWidget


class Modifier():
    mod = 0
    name = ""

MODIFIER_NAMES = []

KEY_BLACKLIST = []

#: Variable keeping the next available window id. Starts at 0.
WINDOW_NB = -1

URL_START = "http://next.atlas.engineer/"

#: A dictionnary of current windows mapping an identifier (int) to a window (Window).
# We keep a dict instead of a list even if identifiers are integers,
# because windows can be deleted.
WINDOWS = {}

def get_window_id():
    global WINDOW_NB
    WINDOW_NB += 1
    return WINDOW_NB

#: A window contains a base widget, a layout, an id (int), a minibuffer.
class Window():
    #: the actual QWidget.
    widget = None
    #: layout, that holds the webview and the minibuffer. We need to
    #save it for later deletion.
    layout = None
    #: window identifier (int, starting from 0).
    identifier = 0
    buffer = None
    minibuffer = None
    #: minibuffer height (px)
    minibuffer_height = 200

    def __init__(self, *args, **kwargs):
        self.widget = QWidget()
        self.layout = QVBoxLayout()
        self.identifier = get_window_id()
        assert self.identifier >= 0

        webview = QWebEngineView()
        QTimer.singleShot(0, partial(webview.setUrl, QUrl(URL_START)))

        minibuffer = QWebEngineView()
        mb_prompt = """
        <html>
        <div> hello minibuffer </div>
        </html>
        """
        QTimer.singleShot(0, partial(minibuffer.setHtml, mb_prompt))

        self.layout.addWidget(webview)
        self.layout.addWidget(minibuffer)

        self.widget.setWindowTitle("Next browser from window {}".format(self.identifier))
        QTimer.singleShot(0, partial(self.widget.setLayout, self.layout))


def window_make():
    """
    Create a window and return its identifier (int).
    """
    # TODO: we can see a window but not its webview and minibuffer.
    window = Window()
    window.widget.show()

    WINDOWS[window.identifier] = window
    print("--- new window created, id {}".format(window.identifier))
    return window.identifier

def window_delete(window_id):
    window = WINDOWS.get(window_id)
    # TODO: passes, but no effect.
    if window.layout is not None:
        # https://stackoverflow.com/questions/9374063/remove-widgets-and-layout-as-well
        while window.layout.count():
            item = window.layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                print("--- deleting widget {}".format(widget))
                widget.deleteLater()

        del window.layout

    del WINDOWS[window_id]
    print("--- deleted window {}. Remaining windows: {}".format(window_id, WINDOWS))
    return True

def window_send_event(widget, event, data):
    pass

def set_title(id, title, **kwargs):
    """
    Set the title of the window of id `id` (str).
    Return the new title if it was changed, a blank string otherwise.
    """
    print("--- set title: {}, {}".format(id, title))
    window = WINDOWS.get(id)
    if window:
        window.base.setWindowTitle(title)
        print("-- title set for window {} !".format(id))
        return title
    return ""
