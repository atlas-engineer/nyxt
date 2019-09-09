# https://wiki.qt.io/Qt_for_Python_DBusIntegration

from PyQt5 import QtDBus
from PyQt5.QtCore import pyqtSlot, Q_CLASSINFO, QObject
from PyQt5.QtWidgets import QApplication


class TestObjectAdaptor(QtDBus.QDBusAbstractAdaptor):
    Q_CLASSINFO("D-Bus Interface", "com.example.TestInterface")
    Q_CLASSINFO("D-Bus Introspection",
                """
                <interface name="com.example.TestInterface">
                    <method name="ping">
                        <arg type="s" name="response" direction="out"/> 
                    </method>
                </interface>
                """)

    def __init__(self, parent):
        super(TestObjectAdaptor, self).__init__(parent)
        self.setAutoRelaySignals(True)

    @pyqtSlot(result=str)
    def ping(self):
        print("In adaptor ping!")
        return self.parent().ping()


class TestObject(QObject):
    def __init__(self):
        super().__init__()
        return

    def ping(self):
        print("In object ping!")
        return 'pong'


if __name__ == '__main__':
    app = QApplication([])

    test_obj = TestObject()
    adapt = TestObjectAdaptor(test_obj)

    session_bus = QtDBus.QDBusConnection.sessionBus()
    session_bus.registerObject("/TestObject", test_obj)
    session_bus.registerService("com.example.TestService")

    app.exec_()
