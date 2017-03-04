"""
"""

import sys

from PyQt5.QtWidgets import QApplication, QHBoxLayout, QWidget
from PyQt5.QtWebKitWidgets import QWebView
from PyQt5.QtCore import QUrl


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
        self.setGeometry(300, 300, 300, 150)
        self.setWindowTitle('Hydra')
        self.show()
        
        
if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Hydra()
    sys.exit(app.exec_())
