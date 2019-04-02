from functools import partial

from PyQt5.QtCore import QUrl
from PyQt5.QtCore import QThread
from PyQt5.QtCore import QTimer
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWidgets import QApplication
from PyQt5.QtWidgets import QPushButton
from PyQt5.QtWidgets import QVBoxLayout
from PyQt5.QtWidgets import QWidget

from xmlrpc.server import SimpleXMLRPCServer

#: xmlrpc port
RPC_PORT = 8082

# Qt
URL_START = "http://next.atlas.engineer/"

app = QApplication([])
window = QWidget()
layout = QVBoxLayout()

webview = QWebEngineView()
webview.setUrl(QUrl(URL_START))

minibuffer = QWebEngineView()
mb_prompt = """
<html>
<div> hello minibuffer </div>
</html>
"""
minibuffer.setHtml(mb_prompt)

layout.addWidget(webview)
layout.addWidget(minibuffer)

window.setWindowTitle("Next browser")
window.setLayout(layout)
window.show()

# xmlrpc
def hello(name):
    # easy: doesn't touch Qt, no need of thread.
    return "hello " + name

def set_minibuffer(name):
    """
    Don't modify a Qt widget directly, but through Qt signals.

    Use the client in another python shell like this:

    from xmlrpc.client import ServerProxy
    client = ServerProxy("http://localhost:8082")
    print(client.set_minibuffer("me"))
    """
    html = mb_prompt.replace("minibuffer", name)
    wrapper = partial(minibuffer.setHtml, html)
    QTimer.singleShot(0, wrapper)
    return html


class RPCThread(QThread):
    def run(self):
        # sleep a little bit to make sure QApplication is running.
        self.sleep(1)
        print("--- starting server…")
        self.rpcserver = SimpleXMLRPCServer(("localhost", RPC_PORT), allow_none=True)
        self.rpcserver.register_function(hello)
        self.rpcserver.register_function(set_minibuffer)

        self.rpcserver.serve_forever()

class RPCWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.thread = RPCThread(self)
        self.thread.start()

rpcwidget = RPCWidget()

# Qt main loop.
print("--- Qt loop")
app.exec_()
