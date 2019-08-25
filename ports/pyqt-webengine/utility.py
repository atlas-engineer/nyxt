import logging
import re
from sys import platform

from PyQt5.QtWebEngineCore import QWebEngineUrlRequestInterceptor, QWebEngineUrlRequestInfo
from PyQt5.QtCore import QCoreApplication, QEvent, Qt, QPoint
from PyQt5.QtGui import QKeyEvent, QKeySequence, QMouseEvent
import window
from core_interface import push_input_event

from PyQt5.QtWidgets import QWidget

import window
from core_interface import push_input_event, request_resource

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
    Qt.Key_Tab: "TAB",
    Qt.Key_Left: "Left",
    Qt.Key_Right: "Right",
    Qt.Key_Up: "Up",
    Qt.Key_Down: "Down",
    Qt.Key_PageUp: "Page_Up",
    Qt.Key_PageDown: "Page_Down",
    Qt.Key_End: "Page_End",
    Qt.Key_Home: "Page_Home",
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
    else:
        if is_control_sequence(event.text()):
            text = QKeySequence(event.key()).toString().lower()
        else:
            try:
                text = chr(event.key()).lower()
            except Exception:
                text = QKeySequence(event.key()).toString().lower()

    return text


def is_control_sequence(s):
    # works on MacOS, returns None on linux.
    return re.match("/(\x9B|\x1B\[)[0-?]*[ -\/]*[@-~]/", s)


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
    logging.info("generate input, window: {} code: {}, modifiers {}, low_level_data {}".format(
        window_id, key_code, modifiers, low_level_data))
    #  Scan Code set to very high value not in system to distinguish
    #  it as an artifical key press, this avoids infinite propagation
    #  of key presses when it is caught by the event filter
    text = None

    if x == -1:
        # Key event.
        if (low_level_data not in SPECIAL_KEYS):
            text = chr(low_level_data)
        event = QKeyEvent(QEvent.KeyPress, key_code, modifiers_flag,
                          10000, 10000, 10000, text=text)
        receiver = window.get_window(window_id).buffer.focusProxy()
        QCoreApplication.sendEvent(receiver, event)

    else:
        # TODO: Mouse event.
        # mouse_event = QMouseEvent()
        point = QPoint(x, y)
        button = Qt.LeftButton
        event = QMouseEvent(QEvent.MouseButtonPress, point, button, button, modifiers_flag)
        event.is_generated = True
        receiver = window.get_window(window_id).buffer.focusProxy()
        logging.info("generate button event: button{}, modifiers {}".format(button, modifiers))
        QCoreApplication.sendEvent(receiver, event)


class EventFilter(QWidget):
    def __init__(self, sender, parent=None):
        super(EventFilter, self).__init__(parent)
        self.sender = sender
        self.sender.installEventFilter(self)

    def eventFilter(self, obj, event):
        if (event.type() == QEvent.KeyPress and not
            is_modifier(event.key()) and
                event.nativeScanCode() != 10000):
            modifiers = create_modifiers_list(event.modifiers())
            key_string = create_key_string(event)
            key_code = event.key()
            low_level_data = 0
            try:
                low_level_data = ord(key_string)
            except TypeError:
                low_level_data = key_code
            except ValueError:
                low_level_data = key_code
            logging.info("send code: {} string: {} modifiers: {} low_level_data: {}".format(
                key_code, key_string, modifiers, low_level_data))
            push_input_event(key_code,
                             key_string,
                             modifiers,
                             -1.0, -1.0, low_level_data,
                             window.active())
            return True

        elif event.type() == QEvent.MouseButtonPress and \
             (hasattr(event, "is_generated") and not event.is_generated):
            modifiers = create_modifiers_list(event.modifiers())
            low_level_data = 0
            button = "button" + str(event.button())
            logging.info("send button press: {}".format(button))
            push_input_event(0,
                             button,
                             modifiers,
                             # relative to the widget that receives the event.
                             # see also globalPos().
                             float(event.x()),
                             float(event.y()),
                             low_level_data,
                             window.active())
            return True

        return False


class WebEngineUrlRequestInterceptor(QWebEngineUrlRequestInterceptor):

    # Buffer identifier (string).
    identifier = "-1"

    def __init__(self, identifier, *args, **kwargs):
        super(WebEngineUrlRequestInterceptor, self).__init__()
        self.identifier = identifier

    def interceptRequest(self, info):
        if platform == "linux" or platform == "linux2":
            url = info.requestUrl().url()
            cookies = ""
            event_type = ""
            is_new_window = False
            is_known_type = True  # if False, download the file.
            if url.endswith("pdf"):
                is_known_type = False
            mouse_button = ""
            modifiers = [""]
            logging.info("Request {}".format(url))
            request_resource(self.identifier, url,
                             cookies,
                             event_type,
                             is_new_window,
                             is_known_type,
                             mouse_button,
                             modifiers)
        elif platform == "darwin":
            # Currently Unimplemented on Darwin. The above, Linux
            # specific code causes an instantaneous crash on Darwin
            pass
