from PyQt5.QtWidgets import QWidget, qApp
import datetime
import logging


class EventFilter(QWidget):
    def __init__(self, parent=None):
        super(EventFilter, self).__init__(parent)
        qApp.installEventFilter(self)

    def eventFilter(self, obj, event):
        logging.info("Event")
        logging.info(datetime.datetime.now())
        return super(EventFilter, self).eventFilter(obj, event)
