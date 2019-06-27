import logging

import buffers
import minibuffer

import dbus
from PyQt5.QtCore import QEvent, Qt, QCoreApplication
from PyQt5.QtWidgets import QVBoxLayout, QWidget
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QKeyEvent



#: A dictionary of current windows mapping an identifier (str) to a window (Window).
WINDOWS = {}

#: Key modifiers.
MODIFIERS = {
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

#: Build modifiers back for Qt.
#: Their enum representation is different from the event's key code.
# https://www.riverbankcomputing.com/static/Docs/PyQt5/api/qtcore/qt-keyboardmodifiers.html
QT_MODIFIERS = {
    'C': Qt.KeyboardModifier.ControlModifier,
    'H': -1,  # TODO:
    'Lock': -1, # TODO:
    'M': Qt.KeyboardModifier.AltModifier,
    'Meta': Qt.KeyboardModifier.MetaModifier,
    's': Qt.KeyboardModifier.ShiftModifier,
    'S': -1,  # TODO:

}

#: Special keys, to be understood by the lisp core.
KEY_TRANSLATIONS = {
    Qt.Key_Backspace: "BACKSPACE",
    Qt.Key_Space: "SPACE",
    Qt.Key_Delete: "DELETE",
    Qt.Key_Escape: "ESCAPE",
    Qt.Key_Return: "RETURN",
    Qt.Key_Tab: "TAB",
    # Qt.Key_Delete: "HYPHEN", # TODO:

    Qt.Key_Right: "Right",
    Qt.Key_Left: "Left",
    Qt.Key_Up: "Up",
    Qt.Key_Down: "Down",
}

CORE_INTERFACE = "engineer.atlas.next.core"
CORE_OBJECT_PATH = "/engineer/atlas/next/core"

def get_window(identifier):
    window = WINDOWS.get(identifier)
    if window:
        return window
    else:
        raise Exception("Window ID: {} not found!".format(identifier))


def is_modifier(key):
    return key in MODIFIERS.keys()

def is_special(key):
    return key in KEY_TRANSLATIONS.keys()

def key_to_special(inputkey):
    for key in KEY_TRANSLATIONS.items():
        if key[1] == inputkey:
            return key[0]

def build_qt_modifiers(names):
    """
    Given a list of strings designing modifiers names ("M", "C"),
    return a KeyboardModifiers class with the internal Qt representation of modifiers.
    If no result, return Qt.NoModifier (i.e, 0) instead.
    """
    res = []
    names = [it for it in names if it != ""]
    for name in names:
        mod = QT_MODIFIERS.get(name)
        if mod is None:
            logging.warn("Unrecognized modifier: {}".format(name))
        elif mod == -1:
            logging.warn("Unsupported modifier: {}".format(name))
            return Qt.NoModifier
        else:
            res.append(mod)
    if res:
        qt_modifiers = Qt.KeyboardModifier(*res)
    else:
        qt_modifiers = Qt.NoModifier
    return qt_modifiers

def build_special_list(names):
    res = []
    for name in names:
        special = key_to_special(name)
        if special:
            res.append(special)
    return res

class KeyCaptureWidget(QWidget):
    """
    Subclass QWidget to catch key presses.
    """
    #: Record consecutive modifier keys.
    modifiers_stack = []
    #: Current event (key and all). We send all input event to the core.
    current_event = None

    #: Identifier (string) of the parent window.
    # XXX: this clearly is sub-optimal: Window.identifier,
    # window.qtwidget being KeyCaptureWidget, KeyCaptureWidget.parent_identifier being window.identifier
    parent_identifier = ""

    # lisp core dbus proxy.
    # Used to send events (push_input_event) asynchronously.
    session_bus = None
    core_dbus_proxy = None

    def __init__(self, identifier="<no id>"):
        self.core_dbus_proxy = self.get_core_dbus_proxy()
        self.parent_identifier = identifier
        super().__init__()

    def get_core_dbus_proxy(self):
        """If it doesn't exist, create a dbus proxy to communicate with the
        lisp core.
        """
        logging.info("session bus again")
        if not self.session_bus:
            self.session_bus = dbus.SessionBus()

        logging.info("and core bus…")
        if not self.core_dbus_proxy:
            self.core_dbus_proxy = self.session_bus.get_object(CORE_INTERFACE, CORE_OBJECT_PATH)
        logging.info("buses ok")
        return self.core_dbus_proxy

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

    def handle_reply(self):
        # push_input_event doesn't receive return values.
        logging.info("async reply received.")

    def handle_error(self, e):
        logging.info("error callback received: {}".format(e))

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
            key_code = event.key()
            key_string = event.text()

            if is_special(key_code):
                key_string = KEY_TRANSLATIONS[key_code]
            else:
                # In case of C-a, text() is "^A", a non-printable character, not what we want.
                # XXX: lower() is necessary, but is it harmless ?
                try:
                    key_string = chr(key_code).lower()  # ascii -> string.
                except Exception:
                    # watch out arrow keys.
                    pass
                logging.debug("our new key_string: from {} to {}".format(event.text(), key_string))

            logging.info("Sending push-input-event with key_code {}, key_string {} and modifiers {}".format(
                key_code, key_string, self.get_modifiers_list()))
            # type signature: int, str, array of strings, double, double, int, str
            self.core_dbus_proxy.push_input_event(
                key_code,  # int
                key_string,
                self.get_modifiers_list(),
                0.0, 0.0,  # TODO: mouse events
                key_code, # low-level-data
                self.parent_identifier,  # sender
                # Give handlers to make the call asynchronous.
                # lambdas don't work.
                reply_handler=self.handle_reply,
                error_handler=self.handle_error,
                dbus_interface=CORE_INTERFACE)

    def get_modifiers_list(self):
        """
        Return the sequence modifiers as a list of strings ("C", "S" etc).
        """
        # dbus always expects an array of strings, not just [].
        # He doesn't know how to encode "None".
        # The lisp core removes empty strings before proceeding.
        return [MODIFIERS[key] for key in self.modifiers_stack] or [""]

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


#: A window contains a window widget, a layout, an id (int), a minibuffer.
class Window():
    #: the actual QWidget.
    qtwindow = None
    #: layout, that holds the buffer and the minibuffer.
    layout = None
    #: window identifier (str)
    identifier = "0"
    #: the buffer
    buffer = None
    #: buffer height (px)
    buffer_height = 480
    #: the minibuffer is an object
    minibuffer = None
    #: minibuffer height (px)
    minibuffer_height = 20

    def __init__(self, identifier=None):
        self.qtwindow = KeyCaptureWidget(identifier=identifier)

        self.layout = QVBoxLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.identifier = identifier

        self.buffer = buffers.Buffer()
        self.buffer.set_height(self.buffer_height)
        self.minibuffer = minibuffer.MiniBuffer()
        self.minibuffer.set_height(self.minibuffer_height)

        self.layout.addWidget(self.buffer.view)
        self.layout.addWidget(self.minibuffer.view)
        self.qtwindow.setLayout(self.layout)
        self.qtwindow.show()

    def set_title(self, title):
        """
        Set the title of the window.
        """
        logging.info("Set title: {}, {}".format(self.identifier, title))
        self.qtwindow.setWindowTitle(title)
        logging.info("Title set for window {} !".format(self.identifier))
        return title

    def set_active_buffer(self, buffer):
        """
        Set the active buffer of the window to buffer.
        FIXME: Results in strange behavior
        """
        self.layout.removeWidget(self.buffer.view)
        self.layout.insertWidget(0, buffer.view)
        self.buffer = buffer
        return True

    def set_minibuffer_height(self, height):
        assert isinstance(height, int)
        self.minibuffer.set_height(height)
        return True

    def minibuffer_evaluate_javascript(self, script):
        self.minibuffer.evaluate_javascript(script)
        return "0"  # TODO: callback ID

    def delete(self):
        # del self.qtwindow
        self.qtwindow.hide()
        return True

    def exists(self):
        if self.qtwindow.isVisible():
            return True


def make(identifier: str):
    """Create a window, assign it the given unique identifier (str).

    We must pass a reference to the lisp core's dbus proxy, in order
    to send asynchronous input events (key, mouse etc).

    return: The Window identifier
    """
    assert isinstance(identifier, str)
    window = Window(identifier=identifier)
    WINDOWS[window.identifier] = window
    logging.info("New window created, id {}".format(window.identifier))
    return identifier


def active():
    """
    Return the active window.
    """
    active_window = QApplication.activeWindow()
    logging.info("Active window by PyQt is {}.".format(active_window))
    for key, value in WINDOWS.items():
        if value.qtwindow == active_window:
            logging.info("Active window id: {}".format(value.identifier))
            return value.identifier
    logging.info("No active window found in {} windows.".format(len(WINDOWS)))


def generate_input_event(window_id, key_code, modifiers, low_level_data, x, y):
    """The lisp core tells us to generate this key event.

    - window_id: str
    - key_code: int
    - modifiers: [str]
    - low_level_data: key code from Qt (int).
    - x, y: float
    """
    qt_modifiers = build_qt_modifiers(modifiers)
    logging.info('generating this input event: window id {}, key code {}, modifiers names {}, \
    modifiers list {}'. format(window_id, key_code, modifiers, qt_modifiers))
    event = QKeyEvent(QEvent.KeyPress, key_code, qt_modifiers)
    QCoreApplication.postEvent(get_window(window_id).qtwindow, event)
