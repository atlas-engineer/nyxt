import logging

import dbus

"""
Call dbus methods on the lisp core side.

We share buses between files (Window, Buffer, etc).
"""

CORE_INTERFACE = "engineer.atlas.next.core"
CORE_OBJECT_PATH = "/engineer/atlas/next/core"

SESSION_BUS = None
CORE_DBUS_PROXY = None


def get_core_dbus_proxy():
    """
    This function is used to create/get a singleton that acts as a proxy
    for the dbus interface.
    """
    global SESSION_BUS
    if not SESSION_BUS:
        SESSION_BUS = dbus.SessionBus()

    global CORE_DBUS_PROXY
    if not CORE_DBUS_PROXY:
        CORE_DBUS_PROXY = SESSION_BUS.get_object(CORE_INTERFACE, CORE_OBJECT_PATH)
    return CORE_DBUS_PROXY


def handle_reply():
    pass


def handle_error(e):
    pass


def push_input_event(key_code, key_string, modifiers_list, x, y, low_level_data, parent_identifier,
                     reply_handler=None, error_handler=None):
    """This function push as input event to the Lisp core.

    :param key_code: integer describing the hardware key code
    :param key_string:
    :param modifiers_list: list of modifier keys
    :param x: mouse x coordinate
    :param y: mouse y coordinate
    :param low_level_data: any Qt specific event data
    :param parent_identifier: the sender of the event
    :param reply_handler:
    :param error_handler:
    :returns: none
    :rtype: none
    """
    proxy = get_core_dbus_proxy()
    proxy.push_input_event(
        key_code,
        key_string,
        modifiers_list,
        x, y,
        low_level_data,
        parent_identifier,
        # Use handlers to make the call asynchronous.
        reply_handler=handle_reply,
        error_handler=handle_error,
        dbus_interface=CORE_INTERFACE)


def buffer_javascript_call_back(identifier, res, callback_id):
    proxy = get_core_dbus_proxy()
    proxy.buffer_javascript_call_back(identifier, res, callback_id,
                                      dbus_interface=CORE_INTERFACE,
                                      # Use handlers to make the call asynchronous.
                                      reply_handler=handle_reply,
                                      error_handler=handle_error)


def minibuffer_javascript_call_back(window_identifier, response, callback_id):
    pass
