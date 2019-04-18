from functools import partial

from PyQt5.QtCore import Qt
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

#: Key translation
key_translations = {
    "^H": "BACKSPACE",
    # TODO:
}

#: Ignore those keys. They are uncommon or useless.
# # TODO: what are ISO_Level3_Shift ? altgr
KEY_BLACKLIST = []

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
        print("-- key: {}".format(event.key()))
        if event.key() == Qt.Key_Control:
            print("-- control key")
        elif event.key() == Qt.Key_Shift:
            print("-- shift key")
        if is_modifier(key):
            self.modifiers_stack.append(key)
            # It's ok Qt, we handled it, don't handle it.
            # return True
        print("-- modifiers: {}".format(self.modifiers_stack))

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
            print("-- modifiers: {}, key: {}".format(self.modifiers_stack, event.key()))
            print("-- send push-key-event now")
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
            print("-- key sequence: {}".format(out))

    def mouseDoubleClickEvent(self, event):
        print("--- double click")

    def mouseReleaseEvent(self, event):
        print("- mouse release")

    def mousePressEvent(self, event):
        print("-- mouse press")


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
        self.widget = MyQWidget()
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
    method_name = "PUSH-KEY-EVENT"

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
