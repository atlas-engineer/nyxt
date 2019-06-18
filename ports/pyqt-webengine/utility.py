import logging
from window import WINDOWS, get_window
from buffers import BUFFERS
# from PyQt5.QtGui import QKeyEvent
# from PyQt5.QtCore import QEvent, Qt, QCoreApplication


# def generate_input_event():
#     print('Key Event')
#     event = QKeyEvent(QEvent.KeyPress, Qt.Key_A, Qt.NoModifier)
#     QCoreApplication.postEvent(window.WINDOWS.get("0").webview, event)


def killall():
    """
    Kill all windows.
    For development.
    """
    ids = list(WINDOWS.keys())  # list, not dict_keys object.
    for id in ids:
        _window = get_window(id)
        _window.delete()
    logging.info("{} windows deleted.".format(len(ids)))


def list_windows():
    """
    Print all windows with their ID.
    For development.
    """
    print("Windows: ")
    for key, val in WINDOWS.items():
        print("{}: {}".format(key, val))


def list_buffers():
    """
    Print all buffers with their ID.
    For development.
    """
    print("Buffers: ")
    for key, val in BUFFERS.items():
        print("{}: {}".format(key, val))
