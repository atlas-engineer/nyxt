
from functools import partial
from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.client import ServerProxy

from PyQt5.QtCore import QThread
from PyQt5.QtCore import QTimer
from PyQt5.QtCore import QObject
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

    (defparameter myinterface (make-instance 'remote-interface))
    (start-interface myinterface)
    ;; "xml-rpc server /RPC2:8081"

Now you can use any built-in methods (window-make myinterface) or send
custom signals with

    (send-signal myinterface "set_minibuffer" "yiha from CL!")

which prints its return value (an html snippet) and which should
change your minibuffer prompt.


You can try the client in another python shell:

    from xmlrpc.client import ServerProxy
    client = ServerProxy("http://localhost:8082")
    print(client.set_minibuffer("me"))

"""

#: xmlrpc port of the platform port
RPC_PORT = 8082

#: xmlrpc port of the lisp core
# TODO: make dynamic
LISP_RPC_PORT = 8081

core_client = ServerProxy("http://localhost:{}".format(LISP_RPC_PORT))
# listMethod and any other method fails with the same error message.
# print(core_client.system.listMethods())
# getattr(core_client, "PUSH-KEY-EVENT")()
# import ipdb; ipdb.set_trace()

app = QApplication([])
# let's just create a window at startup for development.
window_make("0")

mb_prompt = """
<html>
<div> hello minibuffer </div>
</html>
"""

# xmlrpc
def hello(name):
    # easy: doesn't touch Qt, no need of thread.
    return "hello " + name

def set_minibuffer(name):
    # TODO: do in thread.
    """
    Change the minibuffer prompt and return its current html.
    """
    html = mb_prompt.replace("minibuffer", name)
    # wrapper = partial(minibuffer.setHtml, html)
    # QTimer.singleShot(0, wrapper)
    # return html


class RPCThread(QThread):
    """
    Run the rpcxml server in another thread from the (blocking) GUI main loop.
    """
    #: Signals, to create GUI elements into the main Qt thread.
    window_make = pyqtSignal(int)
    window_delete = pyqtSignal(int)

    def run(self):
        """
        Register functions and start the server.
        """
        # sleep a little bit to make sure QApplication is running.
        self.sleep(1)
        print("--- starting server…")
        self.rpcserver = SimpleXMLRPCServer(("localhost", RPC_PORT), allow_none=True)
        self.rpcserver.register_function(hello)
        self.rpcserver.register_function(set_minibuffer)

        # Allow xmlrpc introspection.
        # Use with client.system.listMethods()
        self.rpcserver.register_introspection_functions()
        # Register all functions.
        self.rpcserver.register_function(set_title, "window.set.title")
        # TODO: works, but we don't get the return value anymore.
        self.rpcserver.register_function(partial(self.window_make.emit), "window.make")
        self.rpcserver.register_function(partial(self.window_delete.emit), "window.delete")

        # Serve.
        self.rpcserver.serve_forever()

class RPCWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        # Start the xmlprc thread.
        self.thread = RPCThread(self)
        self.thread.start()

rpcwidget = RPCWidget()
# Connect signals (outside the thread).
rpcwidget.thread.window_make.connect(window_make)
rpcwidget.thread.window_delete.connect(window_delete)

# Qt main loop.
print("--- Qt loop")
app.exec_()
