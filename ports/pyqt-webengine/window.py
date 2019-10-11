import logging

from PyQt5.QtWidgets import QApplication, QVBoxLayout, QWidget

import buffers
import minibuffer
import core_interface

#: A dictionary of current windows mapping an identifier (str) to a window (Window).
WINDOWS = {}


def get_window(identifier):
    window = WINDOWS.get(identifier)
    if window:
        return window
    else:
        raise Exception("Window ID: {} not found!".format(identifier))


#: A window contains a window widget, a layout, an id (int), a minibuffer.
class Window(QWidget):
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

    def __init__(self, identifier=None, parent=None):
        super(Window, self).__init__(parent)
        self.layout = QVBoxLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setSpacing(0)
        self.identifier = identifier

        self.buffer = buffers.Buffer()
        self.minibuffer = minibuffer.Minibuffer(identifier)
        self.minibuffer.set_height(self.minibuffer_height)

        self.layout.addWidget(self.buffer)
        self.layout.addWidget(self.minibuffer)
        self.setLayout(self.layout)
        self.resize(1024, 768)
        self.show()

    def closeEvent(self, event):
        core_interface.window_will_close(self.identifier)
        event.accept()

    def set_title(self, title):
        """
        Set the title of the window.
        """
        self.setWindowTitle(title)
        logging.info("title set for window {}".format(self.identifier))
        return title

    def set_active_buffer(self, buffer):
        """
        Set the active buffer of the window to buffer.
        """
        # Remove the current buffer from the layout (hide it).
        self.buffer.setParent(None)
        self.layout.insertWidget(0, buffer)
        self.buffer = buffer
        return True

    def set_minibuffer_height(self, height):
        assert isinstance(height, int)
        self.minibuffer.set_height(height)
        return True

    def minibuffer_evaluate_javascript(self, script):
        return self.minibuffer.evaluate_javascript(script)

    def delete(self):
        # del self
        self.hide()
        return True

    def exists(self):
        if self.isVisible():
            return True


def make(identifier: str):
    """Create a window, assign it the given unique identifier (str).

    We must pass a reference to the lisp core's dbus proxy, in order
    to send asynchronous input events (key, mouse etc).

    return: The Window identifier
    """
    assert isinstance(identifier, str)
    window = Window(identifier=str(identifier))
    WINDOWS[window.identifier] = window
    logging.info("New window created, id {}".format(window.identifier))
    return identifier


def active():
    """Return the active window.

    """
    active_window = QApplication.activeWindow()
    # The activeWindow might not be of class Window (e.g. it could be a
    # QDialog), so we can't assume it has an identifier member.
    try:
        return active_window.identifier if active_window else "-1"
    except AttributeError:
        return "-1"
