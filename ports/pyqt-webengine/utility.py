import window
from PyQt5.QtGui import QKeyEvent
from PyQt5.QtCore import QEvent, Qt, QCoreApplication


def generate_input_event():
    print('Key Event')
    event = QKeyEvent(QEvent.KeyPress, Qt.Key_A, Qt.NoModifier)
    QCoreApplication.postEvent(window.WINDOWS.get("0").webview, event)
