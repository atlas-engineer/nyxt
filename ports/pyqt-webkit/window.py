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

def get_window_id():
    global WINDOW_NB
    WINDOW_NB += 1
    return WINDOW_NB

#: A window.
class Window():
    #: the actual QWidget.
    base = None
    #: window identifier (int, starting from 0).
    identifier = 0
    buffer = None
    minibuffer = None
    #: minibuffer height (px)
    minibuffer_height = 200

    def __init__(self, *args, **kwargs):
        self.base = QWidget()
        self.identifier = get_window_id()
        assert self.identifier >= 0

#: A dictionnary of current windows mapping an identifier (int) to a window (Window).
# We keep a dict instead of a list even if identifiers are integers,
# because windows can be deleted.
WINDOWS = {}


def make_window():
    """
    Create a window and return its identifier (int).
    """
    URL_START = "http://next.atlas.engineer/"

    window = Window()
    layout = QVBoxLayout()

    webview = QWebEngineView()
    QTimer.singleShot(0, partial(webview.setUrl, QUrl(URL_START)))

    minibuffer = QWebEngineView()
    mb_prompt = """
    <html>
    <div> hello minibuffer </div>
    </html>
    """
    QTimer.singleShot(0, partial(minibuffer.setHtml, mb_prompt))

    layout.addWidget(webview)
    layout.addWidget(minibuffer)

    window.base.setWindowTitle("Next browser from window {}".format(window.identifier))
    QTimer.singleShot(0, partial(window.base.setLayout, layout))
    window.base.show()

    WINDOWS[window.identifier] = window
    print("--- new window created, id {}".format(window.identifier))
    return window.identifier

def window_delete(window):
    pass

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
