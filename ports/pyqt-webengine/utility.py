import logging
from window import WINDOWS, get_window
from buffers import BUFFERS

"""
Utility functions, for development.
"""

def killall():
    """
    Kill all windows.
    For development.
    """
    ids = list(WINDOWS.keys())  # list, not dict_keys object.
    for id in ids:
        _window = get_window(id)
        _window.delete()
    logging.info("{} windows deleted.".format(len(ids)))


def list_windows():
    """
    Print all windows with their ID.
    For development.
    """
    print("Windows: ")
    for key, val in WINDOWS.items():
        print("{}: {}".format(key, val))


def list_buffers():
    """
    Print all buffers with their ID.
    For development.
    """
    print("Buffers: ")
    for key, val in BUFFERS.items():
        print("{}: {}".format(key, val))
