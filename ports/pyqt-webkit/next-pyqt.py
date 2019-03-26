from PyQt5.QtCore import QUrl
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWidgets import QApplication
from PyQt5.QtWidgets import QPushButton
from PyQt5.QtWidgets import QVBoxLayout
from PyQt5.QtWidgets import QWidget

URL_START = "https://next.atlas.engineer/"

app = QApplication([])
window = QWidget()
layout = QVBoxLayout()

webview = QWebEngineView()
webview.setUrl(QUrl(URL_START))
layout.addWidget(webview)
layout.addWidget(QPushButton('Bottom'))

window.setWindowTitle("Next browser")
window.setLayout(layout)
window.show()

app.exec_()
