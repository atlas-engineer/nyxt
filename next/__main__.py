"""
"""

import sys

from PyQt5.QtWidgets import QApplication, QHBoxLayout, QWidget
from PyQt5.QtWebKitWidgets import QWebView
from PyQt5.QtCore import QUrl
from PyQt5.QtCore import Qt


class Hydra(QWidget):
    
    def __init__(self):
        super().__init__()
        self.initUI()
    
    def initUI(self):
        web = QWebView()
        web.load(QUrl("https://www.google.com"))
        
        hbox = QHBoxLayout()
        hbox.addWidget(web)
        
        self.setLayout(hbox)
        self.setWindowTitle('Hydra')
        self.show()
    
    def keyPressEvent(self, e):
        if e.key() == Qt.Key_Escape:
            self.close()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Hydra()
    sys.exit(app.exec_())
