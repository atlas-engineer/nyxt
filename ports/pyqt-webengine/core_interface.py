import dbus
import logging

"""
Call dbus methods on the lisp core side.

We share buses between files (Window, Buffer, etc).
"""

CORE_INTERFACE = "engineer.atlas.next.core"
CORE_OBJECT_PATH = "/engineer/atlas/next/core"

SESSION_BUS = None
CORE_DBUS_PROXY = None

#
# all dbus calls take dbus_interface=CORE_INTERFACE ;)
#

def get_core_dbus_proxy():
    global SESSION_BUS
    if not SESSION_BUS:
        SESSION_BUS = dbus.SessionBus()

    global CORE_DBUS_PROXY
    if not CORE_DBUS_PROXY:
        logging.info("getting lisp core bus")
        CORE_DBUS_PROXY = SESSION_BUS.get_object(CORE_INTERFACE, CORE_OBJECT_PATH)
    return CORE_DBUS_PROXY

def push_input_event(key_code, key_string, modifiers_list, x, y, low_level_data, parent_identifier,
                     reply_handler=None, error_handler=None):
    proxy = get_core_dbus_proxy()
    proxy.push_input_event(
        key_code,  # int
        key_string,
        modifiers_list,
        x, y,  # TODO: mouse events
        low_level_data,
        parent_identifier,  # sender
        # Give handlers to make the call asynchronous.
        # lambdas don't work.
        reply_handler=reply_handler,
        error_handler=error_handler,
        # mandatory:
        dbus_interface=CORE_INTERFACE)

def buffer_javascript_call_back(identifier, res, callback_id):
    proxy = get_core_dbus_proxy()
    proxy.buffer_javascript_call_back(identifier, res, callback_id,
                                      # and still:
                                      dbus_interface=CORE_INTERFACE)
