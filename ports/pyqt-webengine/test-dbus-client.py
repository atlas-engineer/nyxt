# https://wiki.qt.io/Qt_for_Python_DBusIntegration

import sys
from traceback import print_exc

import dbus


def main():
    bus = dbus.SessionBus()
    try:
        proxy_object = bus.get_object("com.example.TestService",
                                      "/com/example/TestService/object")
        hello_reply = proxy_object.ping("Hello from example-client.py!",
                                        dbus_interface="com.example.TestInterface")
    except dbus.DBusException:
        print_exc()
        sys.exit(1)
    print(hello_reply)


if __name__ == '__main__':
    main()
