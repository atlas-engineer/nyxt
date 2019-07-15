import logging

import buffers
import minibuffer
import utility

from PyQt5.QtWidgets import QApplication, QVBoxLayout, QWidget


#: A dictionary of current windows mapping an identifier (str) to a window (Window).
WINDOWS = {}


def get_window(identifier):
    window = WINDOWS.get(identifier)
    if window:
        return window
    else:
        raise Exception("Window ID: {} not found!".format(identifier))


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
        self.qtwindow = QWidget()
        self.layout = QVBoxLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.identifier = identifier

        self.buffer = buffers.Buffer()
        self.buffer.set_height(self.buffer_height)
        self.minibuffer = minibuffer.Minibuffer(identifier)
        self.minibuffer.set_height(self.minibuffer_height)
        self.event_filter = utility.EventFilter(self.qtwindow, self.identifier)

        self.layout.addWidget(self.buffer.view)
        self.layout.addWidget(self.minibuffer.view)
        self.qtwindow.setLayout(self.layout)
        self.qtwindow.resize(1024, 768)
        self.qtwindow.show()

    def set_title(self, title):
        """
        Set the title of the window.
        """
        self.qtwindow.setWindowTitle(title)
        logging.info("Title set for window {} !".format(self.identifier))
        return title

    def set_active_buffer(self, buffer):
        """
        Set the active buffer of the window to buffer.
        """
        # Remove the current buffer from the layout (hide it).
        self.buffer.view.setParent(None)
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
    for key, value in WINDOWS.items():
        if value.qtwindow == active_window:
            return value.identifier
    logging.info("No active window found in {} windows.".format(len(WINDOWS)))
