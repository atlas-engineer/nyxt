import logging
from sys import platform

from core_interface import push_input_event

from PyQt5.QtCore import QEvent, Qt
from PyQt5.QtWidgets import QWidget, qApp
from PyQt5.QtGui import QKeySequence

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

if platform == "linux" or platform == "linux2":
    tmp = {Qt.ShiftModifier: "s",
           Qt.ControlModifier: "C",
           Qt.AltModifier: "S",
           Qt.MetaModifier: "M"}
    MODIFIERS.update(tmp)
elif platform == "darwin":
    tmp = {Qt.ShiftModifier: "s",
           Qt.ControlModifier: "S",
           Qt.AltModifier: "M",
           Qt.MetaModifier: "C"}
    MODIFIERS.update(tmp)
elif platform == "win32" or platform == "win64":
    tmp = {Qt.ShiftModifier: "s",
           Qt.ControlModifier: "C",
           Qt.AltModifier: "S",
           Qt.MetaModifier: "M"}
    MODIFIERS.update(tmp)


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
        logging.info("special")
    elif event.text():
        text = event.text()
        logging.info("text")
    else:
        text = QKeySequence(event.key()).toString().lower()
        logging.info("normal")
    return text


def is_modifier(key):
    return key in MODIFIER_KEYS.keys()


class EventFilter(QWidget):
    def __init__(self, parent=None):
        super(EventFilter, self).__init__(parent)
        qApp.installEventFilter(self)

    def eventFilter(self, obj, event):
        if (event.type() == QEvent.KeyPress and not is_modifier(event.key())):
            modifiers = create_modifiers_list(event.modifiers())
            key_string = create_key_string(event)
            key_code = event.key()
            logging.info("code: {} string: {} modifiers {}".format(
                key_code, key_string, modifiers))
            push_input_event(key_code,
                             key_string,
                             modifiers,
                             -1.0, -1.0, key_code,
                             "1")
            return True
        return False
