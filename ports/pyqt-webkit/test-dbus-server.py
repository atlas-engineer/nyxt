usage = """Usage:
python example-signal-emitter.py &
python example-signal-recipient.py
python example-signal-recipient.py --exit-service
"""

from PyQt5.QtWidgets import QApplication
from dbus.mainloop.pyqt5 import DBusQtMainLoop

import dbus
import dbus.service


class TestObject(dbus.service.Object):
    def __init__(self, conn, object_path='/com/example/TestService/object'):
        dbus.service.Object.__init__(self, conn, object_path)

    @dbus.service.method('com.example.TestInterface')
    def Ping(self):
        return 'pong'


if __name__ == '__main__':
    DBusQtMainLoop(set_as_default=True)

    session_bus = dbus.SessionBus()
    name = dbus.service.BusName('com.example.TestService', session_bus)
    object = TestObject(session_bus)

    app = QApplication([])
    app.exec_()
    print(usage)
