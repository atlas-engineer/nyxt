import logging

import buffers
import minibuffer

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QVBoxLayout, QWidget
from PyQt5.QtWidgets import QApplication


#: A dictionary of current windows mapping an identifier (str) to a window (Window).
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

CORE_INTERFACE = "engineer.atlas.next.core"


def get_window(identifier):
    window = WINDOWS.get(identifier)
    if window:
        return window
    else:
        raise Exception("Window ID: {} not found!".format(identifier))


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

    #: Identifier (string) of the parent window.
    parent_identifier = ""

    # lisp core dbus proxy.
    # Used to send events (push_input_event) asynchronously.
    core_dbus_proxy = None

    def __init__(self, core_dbus_proxy=None, identifier=""):
        self.core_dbus_proxy = core_dbus_proxy
        self.parent_identifier = identifier
        super().__init__()

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
        logging.info("async reply received: {}".format(r))

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
            logging.info("Here we would send push-key-event with key {} and modifiers {}".format(
                event.key(), self.get_modifiers_list()))
            # type signature: int, str, array of strings, double, double, int, str
            # self.core_dbus_proxy.push_input_event(
            #     event.key(),
            #     event.text(),
            #     self.get_modifiers_list(),
            #     0.0, 0.0, 0,
            #     self.parent_identifier,  # sender
            #     # Give handlers to make the call asynchronous.
            #     # lambdas don't work.
            #     reply_handler=self.handle_reply,
            #     error_handler=self.handle_error,
            #     dbus_interface=CORE_INTERFACE)

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
        self.qtwindow = MyQWidget()

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
