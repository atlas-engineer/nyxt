from functools import partial
from dbus.mainloop.pyqt5 import DBusQtMainLoop

from PyQt5.QtCore import QThread
from PyQt5.QtCore import pyqtSignal
from PyQt5.QtWidgets import QApplication
from PyQt5.QtWidgets import QWidget

from window import window_make
from window import window_delete
from window import window_send_event
from window import set_title

"""
This is a Next port with Qt's Web Engine, through PyQt.

An important thing is to not modify a Qt widget directly, but through Qt signals.

It is possible to test this from the Python or the Lisp REPL.

To send signals to the web engine from Lisp:
- start the PyQt port (make run)
- start lisp, quickload next
- create an interface and start it:

Now you can use any built-in methods (window-make myinterface) or send
custom signals with

    (send-signal myinterface "set_minibuffer" "yiha from CL!")

which prints its return value (an html snippet) and which should
change your minibuffer prompt.

"""


def client_run(client, cmd, *args):
    pass


def set_minibuffer(name):
    # TODO: do in thread.
    """
    Change the minibuffer prompt and return its current html.
    """
    mb_prompt = """
    <html>
    <div> hello minibuffer </div>
    </html>
    """
    html = mb_prompt.replace("minibuffer", name)
    # wrapper = partial(minibuffer.setHtml, html)
    # QTimer.singleShot(0, wrapper)
    return html


if __name__ == '__main__':
    DBusQtMainLoop(set_as_default=True)
    app = QApplication([])
    window_make("0")
    app.exec_()
