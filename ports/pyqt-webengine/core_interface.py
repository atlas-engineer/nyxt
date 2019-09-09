from PyQt5 import QtDBus
from PyQt5.QtCore import QMetaType
"""
Call dbus methods asynchronously on the Lisp core side.
"""

#
# All dbus calls take:
# - dbus_interface=CORE_INTERFACE
# - a reply_handler and error_handler for the call to be async.
#

CORE_INTERFACE = "engineer.atlas.next.core"
CORE_OBJECT_PATH = "/engineer/atlas/next/core"

SESSION_BUS = None
QDBUS_IFACE = None


def pack_to_strarray(input_list):
    """
    Pack a list into a QDBusArgument that will get sent over the dbus as a
    proper array of strings (sending a list of strs naively will result in a
    list of variants)
    """
    return QtDBus.QDBusArgument(input_list, QMetaType.QStringList)


def get_core_dbus_iface():
    """
    This function is used to create/get a singleton that acts as a proxy
    for the dbus interface.
    """
    global SESSION_BUS
    if not SESSION_BUS:
        SESSION_BUS = QtDBus.QDBusConnection.sessionBus()

    global QDBUS_IFACE
    if not QDBUS_IFACE:
        QDBUS_IFACE = QtDBus.QDBusInterface(CORE_INTERFACE, CORE_OBJECT_PATH,
                                            CORE_INTERFACE, SESSION_BUS)

    return QDBUS_IFACE


# def handle_reply():
#     pass


# def handle_error(e):
#     pass


def push_input_event(key_code, key_string, modifiers_list, x, y,
                     low_level_data, parent_identifier):
    """This function push as input event to the Lisp core.

    :param key_code: integer describing the hardware key code
    :param key_string:
    :param modifiers_list: list of modifier keys (list of strings)
    :param x: mouse x coordinate (float)
    :param y: mouse y coordinate
    :param low_level_data: any Qt specific event data (str)
    :param parent_identifier: the sender of the event (str)
    :returns: none
    :rtype: none
    """
    iface = get_core_dbus_iface()
    print("Input event!")
    print("modifiers_list = ", modifiers_list)
    print("arrayed version: ", pack_to_strarray(modifiers_list))

    iface.asyncCall("push_input_event",
                    key_code,
                    key_string,
                    pack_to_strarray(modifiers_list),
                    x, y,
                    low_level_data,
                    parent_identifier)

    # proxy.push_input_event(
    #     key_code,
    #     key_string,
    #     modifiers_list,
    #     x, y,
    #     low_level_data,
    #     parent_identifier,
    #     # Use handlers to make the call asynchronous.
    #     reply_handler=handle_reply,
    #     error_handler=handle_error,
    #     dbus_interface=CORE_INTERFACE)


def buffer_javascript_call_back(identifier, res, callback_id):
    iface = get_core_dbus_iface()
    iface.asyncCall("buffer_javascript_call_back",
                    identifier, res, callback_id)

    return


def buffer_did_commit_navigation(identifier, url):
    iface = get_core_dbus_iface()
    iface.asyncCall("buffer_did_commit_navigation",
                    identifier, url)

    return


def buffer_did_finish_navigation(identifier, url):
    iface = get_core_dbus_iface()
    iface.asyncCall("buffer_did_finish_navigation",
                    identifier, url)

    return


def minibuffer_javascript_call_back(window_identifier, response, callback_id):
    iface = get_core_dbus_iface()
    iface.asyncCall("minibuffer_javascript_call_back",
                    window_identifier, response, callback_id)

    return


def window_will_close(window_identifier):
    iface = get_core_dbus_iface()
    iface.asyncCall("window_will_close", window_identifier)

    return


# def handle_request_resource(handle_p):
#     return handle_p


def request_resource(buffer_identifier, url,
                     cookies, event_type, is_new_window,
                     is_known_type, mouse_button, modifiers):
    iface = get_core_dbus_iface()
    iface.asyncCall("request_resource",
                    buffer_identifier, url, cookies, event_type, is_new_window,
                    is_known_type, mouse_button, pack_to_strarray(modifiers))

    return
