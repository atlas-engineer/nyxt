import logging
from sys import platform

import window
from core_interface import push_input_event

from PyQt5.QtCore import QEvent, Qt, QCoreApplication
from PyQt5.QtGui import QKeyEvent, QKeySequence
from PyQt5.QtWidgets import QWidget

# Used to detect if a keypress was just a modifier
MODIFIER_KEYS = {
    Qt.Key_Shift: "s",
    Qt.Key_Control: "C",
    Qt.Key_Alt: "M",
    Qt.Key_AltGr: "M",
    Qt.Key_Meta: "Meta",
    Qt.Key_Super_L: "S",
    Qt.Key_Super_R: "S"
}

# Special keys for Next
SPECIAL_KEYS = {
    Qt.Key_Backspace: "BACKSPACE",
    Qt.Key_Delete: "DELETE",
    Qt.Key_Escape: "ESCAPE",
    Qt.Key_hyphen: "HYPHEN",
    Qt.Key_Return: "RETURN",
    Qt.Key_Enter: "RETURN",
    Qt.Key_Space: "SPACE",
    Qt.Key_Tab: "TAB"
}

# Used for bitmasking to determine modifiers
MODIFIERS = {}
# Used for constructing a bitmasked modifier
REVERSE_MODIFIERS = {}

if platform == "linux" or platform == "linux2":
    tmp = {Qt.ShiftModifier: "s",
           Qt.ControlModifier: "C",
           Qt.AltModifier: "M",
           Qt.MetaModifier: "M"}
    MODIFIERS.update(tmp)
    tmp = {"s": Qt.ShiftModifier,
           "C": Qt.ControlModifier,
           "M": Qt.AltModifier,
           "M": Qt.MetaModifier}
    REVERSE_MODIFIERS.update(tmp)
elif platform == "darwin":
    tmp = {Qt.ShiftModifier: "s",
           Qt.ControlModifier: "S",
           Qt.AltModifier: "M",
           Qt.MetaModifier: "C"}
    MODIFIERS.update(tmp)
    tmp = {"s": Qt.ShiftModifier,
           "S": Qt.ControlModifier,
           "M": Qt.AltModifier,
           "C": Qt.MetaModifier}
    REVERSE_MODIFIERS.update(tmp)
elif platform == "win32" or platform == "win64":
    tmp = {Qt.ShiftModifier: "s",
           Qt.ControlModifier: "C",
           Qt.AltModifier: "M",
           Qt.MetaModifier: "M"}
    MODIFIERS.update(tmp)
    tmp = {"s": Qt.ShiftModifier,
           "C": Qt.ControlModifier,
           "M": Qt.AltModifier,
           "M": Qt.MetaModifier}
    REVERSE_MODIFIERS.update(tmp)


def create_modifiers_list(event_modifiers):
    modifiers = []
    for key, value in MODIFIERS.items():
        if (event_modifiers & key):
            modifiers.append(value)
    return modifiers or [""]


def create_key_string(event):
    text = ""
    if event.key() in SPECIAL_KEYS:
        text = SPECIAL_KEYS.get(event.key())
    elif event.text():
        text = event.text()
    else:
        text = QKeySequence(event.key()).toString().lower()
    return text


def create_modifiers_flag(modifiers):
    flag = Qt.KeyboardModifiers()
    for modifier in modifiers:
        if(REVERSE_MODIFIERS.get(modifier)):
            flag = flag | REVERSE_MODIFIERS.get(modifier)
    return flag


def is_modifier(key):
    return key in MODIFIER_KEYS.keys()


def generate_input_event(window_id, key_code, modifiers, low_level_data, x, y):
    """
    The Lisp core tells us to generate this key event.

    - window_id: str
    - key_code: int
    - modifiers: [str]
    - low_level_data: key code from Qt (int).
    - x, y: float
    """
    modifiers_flag = create_modifiers_flag(modifiers)
    logging.info("generate input, window: {} code: {}, modifiers {}".format(
        window_id, key_code, modifiers_flag))
    event = QKeyEvent(QEvent.KeyPress, key_code, modifiers_flag, text="-1")
    receiver = window.get_window(window_id).buffer.view.focusProxy()
    QCoreApplication.sendEvent(receiver, event)


class EventFilter(QWidget):
    def __init__(self, sender, identifier, parent=None):
        super(EventFilter, self).__init__(parent)
        self.sender = sender
        self.sender.installEventFilter(self)
        self.identifier = identifier

    def eventFilter(self, obj, event):
        if (event.type() == QEvent.KeyPress and not
            is_modifier(event.key()) and not
            event.text() == "-1"):
            modifiers = create_modifiers_list(event.modifiers())
            key_string = create_key_string(event)
            key_code = event.key()
            logging.info("send code: {} string: {} modifiers {}".format(
                key_code, key_string, modifiers))
            push_input_event(key_code,
                             key_string,
                             modifiers,
                             -1.0, -1.0, key_code,
                             self.identifier)
            return True
        elif (event.type() == QEvent.KeyPress and
              event.text() == "-1"):
            # If we made it to here, an event was sent to the webview
            # but was not consumed, and bubbled up to self.sender (the
            # Qt window), to avoid infinite propagation, don't keep
            # sending the event, say that we handled it
            return True
        return False
