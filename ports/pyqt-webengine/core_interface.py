import dbus

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


def push_input_event(key_code, key_string, modifiers_list, x, y, low_level_data, parent_identifier):
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


def buffer_did_commit_navigation(identifier, url):
    proxy = get_core_dbus_proxy()
    proxy.buffer_did_commit_navigation(identifier, url,
                                       dbus_interface=CORE_INTERFACE,
                                       # Use handlers to make the call asynchronous.
                                       reply_handler=handle_reply,
                                       error_handler=handle_error)


def buffer_did_finish_navigation(identifier, url):
    proxy = get_core_dbus_proxy()
    proxy.buffer_did_finish_navigation(identifier, url,
                                       dbus_interface=CORE_INTERFACE,
                                       # Use handlers to make the call asynchronous.
                                       reply_handler=handle_reply,
                                       error_handler=handle_error)


def minibuffer_javascript_call_back(window_identifier, response, callback_id):
    proxy = get_core_dbus_proxy()
    proxy.minibuffer_javascript_call_back(window_identifier, response, callback_id,
                                          dbus_interface=CORE_INTERFACE,
                                          # Use handlers to make the call asynchronous.
                                          reply_handler=handle_reply,
                                          error_handler=handle_error)


def window_will_close(window_identifier):
    proxy = get_core_dbus_proxy()
    proxy.window_will_close(window_identifier,
                            dbus_interface=CORE_INTERFACE,
                            # Use handlers to make the call asynchronous.
                            reply_handler=handle_reply,
                            error_handler=handle_error)


def handle_request_resource(handle_p):
    return handle_p

def request_resource(buffer_identifier, url,
                     cookies, event_type, is_new_window,
                     is_known_type, mouse_button, modifiers):
    proxy = get_core_dbus_proxy()
    proxy.request_resource(buffer_identifier, url,
                           cookies, event_type, is_new_window,
                           is_known_type, mouse_button, modifiers,
                           # Use handlers to make the call asynchronous.
                           dbus_interface=CORE_INTERFACE,
                           reply_handler=handle_request_resource,
                           error_handler=handle_error)
