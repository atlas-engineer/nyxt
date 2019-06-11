import logging
from PyQt5.QtCore import Qt
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

#: A dictionnary of current windows mapping an identifier (str) to a window (Window).
# Do we want QMutex ?
WINDOWS = {}

#: Key modifiers.
modifiers = {
    Qt.Key_Shift: "s",
    Qt.Key_Control: "C",
    Qt.Key_CapsLock: "Lock",
    Qt.Key_Alt: "M",
    Qt.Key_AltGr: "M",
    Qt.Key_Meta: "Meta",
    Qt.Key_Super_L: "S",
    Qt.Key_Super_R: "S",
    Qt.Key_Hyper_L: "H",
    Qt.Key_Hyper_R: "H",
}


def is_modifier(key):
    return key in modifiers.keys()


class MyQWidget(QWidget):
    """
    Subclass QWidget to catch key presses.
    """
    #: Record consecutive modifier keys.
    modifiers_stack = []
    #: Current event (key and all). We send all input event to the core.
    current_event = None

    def keyPressEvent(self, event):
        key = event.key()
        logging.info("-- key: {}".format(event.key()))
        if event.key() == Qt.Key_Control:
            logging.info("-- control key")
        elif event.key() == Qt.Key_Shift:
            logging.info("-- shift key")
        if is_modifier(key):
            self.modifiers_stack.append(key)
            # It's ok Qt, we handled it, don't handle it.
            # return True
        logging.info("-- modifiers: {}".format(self.modifiers_stack))

    def keyReleaseEvent(self, event):
        """
        Send all input events with a list of modifier keys.
        """
        # We want the list of modifiers when C-S-a is typed.  We might
        # have it with Qt.Keyboardmodifier() or event.modifiers() but
        # they don't return useful values so far. This works.
        if is_modifier(event.key()):
            self.modifiers_stack = []
            self.current_event = None
            # return True  # fails
        else:
            self.current_event = event
            logging.info("-- modifiers: {}, key: {}".format(self.modifiers_stack, event.key()))
            logging.info("-- send push-key-event now")
        self.get_key_sequence()

    def get_modifiers_list(self):
        """
        Return the sequence modifiers as a list of strings ("C", "S" etc).
        """
        return [modifiers[key] for key in self.modifiers_stack]

    def get_key_sequence(self):
        """Return a string representing the sequence: keycode, keyval,
        list of modifiers, low level data, window id.
        """
        if self.current_event:
            out = "{}, {}, {}, {}, {}".format(
                "keycode",
                self.current_event.key(),
                self.get_modifiers_list(),
                "low level data",
                "window id",
            )
            logging.info("-- key sequence: {}".format(out))

    def mouseDoubleClickEvent(self, event):
        logging.info("--- double click")

    def mouseReleaseEvent(self, event):
        logging.info("- mouse release")

    def mousePressEvent(self, event):
        logging.info("-- mouse press")


#: A window contains a base widget, a layout, an id (int), a minibuffer.
class Window():
    #: the actual QWidget.
    widget = None
    #: layout, that holds the webview and the minibuffer. We need to
    #  save it for later deletion.
    layout = None
    #: window identifier (str)
    identifier = "0"
    buffer = None
    #: the minibuffer is a webview too
    minibuffer = None
    #: minibuffer height (px)
    minibuffer_height = 200
    webview = None

    def __init__(self, identifier, *args, **kwargs):
        minibuffer_size = 150
        self.widget = MyQWidget()
        self.layout = QVBoxLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)
        identifier = identifier
        self.identifier = identifier

        self.webview = QWebEngineView()
        self.webview.setUrl(QUrl(URL_START))

        self.minibuffer = QWebEngineView()
        default_prompt = """
        <html>
        <div> hello minibuffer </div>
        </html>
        """
        self.minibuffer.setHtml(default_prompt)
        self.minibuffer.setFixedHeight(minibuffer_size)

        self.layout.addWidget(self.webview)
        self.layout.addWidget(self.minibuffer)

        self.widget.setWindowTitle("Next browser from window {}".format(self.identifier))
        self.widget.setLayout(self.layout)

    def set_minibuffer(self, text, *args, **kwargs):
        self.minibuffer.setHtml(text)

    def set_minibuffer_height(self, height):
        self.minibuffer.setFixedHeight(height)


def window_make(identifier):
    """
    Create a window, assign it the given unique identifier (str).

    return: True (not important)
    """
    assert isinstance(identifier, str)
    window = Window(identifier=identifier)
    window.widget.show()
    WINDOWS[window.identifier] = window
    logging.info("--- new window created, id {}".format(window.identifier))
    return identifier


def window_delete(identifier):
    assert isinstance(identifier, str)
    window = WINDOWS.get(identifier)
    # xxx: works, re-check if everything's necessary.
    if not window:
        logging.info("could not find a window of id {}".format(identifier))
        logging.info("windows: {}".format(WINDOWS))
        return False
    if window.layout is not None:
        # https://stackoverflow.com/questions/9374063/remove-widgets-and-layout-as-well
        while window.layout.count():
            item = window.layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                logging.info("--- deleting widget {}".format(widget))
                widget.deleteLater()

        del window.layout

    del WINDOWS[identifier]
    logging.info("--- deleted window {}. Remaining windows: {}".format(identifier, WINDOWS))
    return True

def window_send_event(widget, event, data):
    method_name = "PUSH-KEY-EVENT"

def window_killall():
    ids = list(WINDOWS.keys())  # list, not dict_keys object.
    length = len(ids)
    for id in ids:
        window_delete(id)
    logging.info("-- {} windows deleted.".format(length))

def set_title(identifier, title, **kwargs):
    """
    Set the title of the window of identifier `identifier` (str).
    Return the new title if it was changed, a blank string otherwise.
    """
    logging.info("--- set title: {}, {}".format(identifier, title))
    assert isinstance(identifier, str)
    window = WINDOWS.get(identifier)
    if window:
        window.widget.setWindowTitle(title)
        logging.info("-- title set for window {} !".format(identifier))
        return title
    return ""

def set_minibuffer(identifier, text):
    """
    Set the minibuffer of the window of id `identifier`.
    """
    assert isinstance(identifier, str)
    window = WINDOWS.get(identifier)
    if window:
        window.set_minibuffer(text)
    else:
        logging.info("--- no window of id {}".format(identifier))

def set_minibuffer_height(identifier, height):
    assert isinstance(identifier, str)
    assert isinstance(height, int)
    window = WINDOWS.get(identifier)
    if window:
        window.set_minibuffer_height(height)
    else:
        logging.info("--- no window of id {}".format(identifier))
