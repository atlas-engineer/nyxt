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

URL_START = "http://next.atlas.engineer/"

#: A dictionnary of current windows mapping an identifier (int) to a window (Window).
# We keep a dict instead of a list even if identifiers are integers,
# because windows can be deleted.
# xxx: we probably want QMutex.
WINDOWS = {}

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

    def __init__(self, identifier, *args, **kwargs):
        self.widget = QWidget()
        self.layout = QVBoxLayout()
        identifier = int(identifier)
        self.identifier = identifier

        webview = QWebEngineView()
        webview.setUrl(QUrl(URL_START))

        minibuffer = QWebEngineView()
        mb_prompt = """
        <html>
        <div> hello minibuffer </div>
        </html>
        """
        minibuffer.setHtml(mb_prompt)

        self.layout.addWidget(webview)
        self.layout.addWidget(minibuffer)

        self.widget.setWindowTitle("Next browser from window {}".format(self.identifier))
        self.widget.setLayout(self.layout)


def window_make(identifier):
    """
    Create a window, assign it the given unique identifier.
    - uid: string

    return: True (not important)
    """
    window = Window(identifier=identifier)
    window.widget.show()
    WINDOWS[window.identifier] = window
    print("--- new window created, id {}".format(window.identifier))
    return True

def window_delete(window_id):
    window = WINDOWS.get(window_id)
    # xxx: works, re-check if everything's necessary.
    if not window:
        print("could not find a window of id {}".format(window_id))
        print("windows: {}".format(WINDOWS))
        return False
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
