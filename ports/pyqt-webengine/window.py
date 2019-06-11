import logging

import minibuffer

from PyQt5.QtCore import Qt, QUrl
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWidgets import QVBoxLayout, QWidget


class Modifier():
    mod = 0
    name = ""

MODIFIER_NAMES = []
KEY_BLACKLIST = []
URL_START = "http://next.atlas.engineer/"

#: A dictionnary of current windows mapping an identifier (str) to a window (Window).
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
        logging.info("Key: {}".format(event.key()))
        if event.key() == Qt.Key_Control:
            logging.info("Control key")
        elif event.key() == Qt.Key_Shift:
            logging.info("Shift key")
        if is_modifier(key):
            self.modifiers_stack.append(key)
            # It's ok Qt, we handled it, don't handle it.
            # return True
        logging.info("Modifiers: {}".format(self.modifiers_stack))

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
            logging.info("Modifiers: {}, key: {}".format(self.modifiers_stack, event.key()))
            logging.info("Send push-key-event now")
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
            logging.info("Key sequence: {}".format(out))

    def mouseDoubleClickEvent(self, event):
        logging.info("Double click")

    def mouseReleaseEvent(self, event):
        logging.info("Mouse release")

    def mousePressEvent(self, event):
        logging.info("Mouse press")


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
    #: the minibuffer is an object
    minibuffer = None
    #: minibuffer height (px)
    minibuffer_height = 20
    webview = None

    def __init__(self, identifier, *args, **kwargs):
        self.widget = MyQWidget()
        self.layout = QVBoxLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.identifier = identifier

        self.webview = QWebEngineView()
        self.webview.setUrl(QUrl(URL_START))

        self.minibuffer = minibuffer.MiniBuffer()
        self.minibuffer.set_height(self.minibuffer_height)

        self.layout.addWidget(self.webview)
        self.layout.addWidget(self.minibuffer.view)
        self.widget.setLayout(self.layout)


def make(identifier: str):
    """
    Create a window, assign it the given unique identifier (str).

    return: The Window identifier
    """
    assert isinstance(identifier, str)
    window = Window(identifier=identifier)
    window.widget.show()
    WINDOWS[window.identifier] = window
    logging.info("New window created, id {}".format(window.identifier))
    return identifier


def delete(identifier):
    assert isinstance(identifier, str)
    window = WINDOWS.get(identifier)

    if not window:
        logging.info("Could not find a window of id {}".format(identifier))
        logging.info("Windows: {}".format(WINDOWS))
        return False
    # Do not delete all widgets, they are transient of windows.
    if window.layout is not None:
        # https://stackoverflow.com/questions/9374063/remove-widgets-and-layout-as-well
        while window.layout.count():
            item = window.layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                logging.info("Deleting widget {}".format(widget))
                widget.deleteLater()

        del window.layout

    del WINDOWS[identifier]
    logging.info("Deleted window {}. Remaining windows: {}".format(identifier, WINDOWS))
    return True


def exists(identifier):
    return True


def killall():
    ids = list(WINDOWS.keys())  # list, not dict_keys object.
    length = len(ids)
    for id in ids:
        delete(id)
    logging.info("{} windows deleted.".format(length))


def set_title(identifier, title, **kwargs):
    """
    Set the title of the window of identifier `identifier` (str).
    Return the new title if it was changed, a blank string otherwise.
    """
    logging.info("Set title: {}, {}".format(identifier, title))
    assert isinstance(identifier, str)
    window = WINDOWS.get(identifier)
    if window:
        window.widget.setWindowTitle(title)
        logging.info("Title set for window {} !".format(identifier))
        return title
    logging.warning("Unable to set title for window {}, could not find window.".format(identifier))
    return ""


def set_minibuffer_height(identifier, height):
    assert isinstance(identifier, str)
    assert isinstance(height, int)
    window = WINDOWS.get(identifier)
    if window:
        window.minibuffer.set_height(height)
    else:
        logging.warning("No window for id: {}".format(identifier))


def minibuffer_evaluate_javascript(identifier, script):
    window = WINDOWS.get(identifier)
    if window:
        window.minibuffer.evaluate_javascript(script)
    else:
        logging.warning("No window for id: {}".format(identifier))
