from PyQt5.QtWidgets import QWidget, qApp
from PyQt5.QtCore import QEvent, Qt
from sys import platform
import logging

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
    return modifiers


def is_modifier(key):
    return key in MODIFIER_KEYS.keys()


class EventFilter(QWidget):
    def __init__(self, parent=None):
        super(EventFilter, self).__init__(parent)
        qApp.installEventFilter(self)

    def eventFilter(self, obj, event):
        if (event.type() == QEvent.KeyPress and not is_modifier(event.key())):
            modifiers = create_modifiers_list(event.modifiers())
            logging.info(event.key())
            logging.info(modifiers)
            return True
        return False
