usage = """Usage:
python example-service.py &
python example-client.py
python example-client.py --exit-service
"""

import sys
from traceback import print_exc
import dbus


def main():
    bus = dbus.SessionBus()

    try:
        proxy_object = bus.get_object("com.example.TestService",
                                      "/com/example/TestService/object")
        hello_reply = proxy_object.Ping("Hello from example-client.py!",
                                        dbus_interface="com.example.TestInterface")
    except dbus.DBusException:
        print_exc()
        print(usage)
        sys.exit(1)

    print(hello_reply)


if __name__ == '__main__':
    main()
