import sys

from xmlrpc.client import ServerProxy

"""
Simple code to send xmlprc signals to the running server.
For development.
"""

if __name__ == "__main__":

    with ServerProxy("http://localhost:8082") as client:
        if "-d" in sys.argv:
            window_id = int(sys.argv[-1])
            # We must use getattr because of dotted function names.
            print(getattr(client, "window.delete")(window_id))
        else:
            print(client.system.listMethods())
            print("-- window make:")
            print(getattr(client, "window.make")())
