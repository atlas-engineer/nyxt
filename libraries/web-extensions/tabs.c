// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "tabs.h"

static JSCValue *
tabs_query_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.query", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

static JSCValue *
tabs_create_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.create", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

static JSCValue *
tabs_get_current_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.getCurrent", NULL);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

static JSCValue *
tabs_get_callback (JSCValue *id)
{
        GVariant *variant = g_variant_new("ms", jsc_value_to_string(id));
        WebKitUserMessage *message = webkit_user_message_new("tabs.get", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, jsc_context_get_current(), i);
}

static void
tabs_print_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.print", NULL);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
}

static JSCValue *
tabs_send_message_callback (GPtrArray *args, void *extension_id)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *tab_id = args->pdata[0];
        JSCValue *object = args->pdata[1];
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        jsc_value_object_set_property(wrapper, "tabId", tab_id);
        jsc_value_object_set_property(wrapper, "message", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.sendMessage", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

static JSCValue *
tabs_insert_css_callback (GPtrArray *args, void *extension_id)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *tab_id = (args->len == 1 ?
                            jsc_value_new_number(context, 0) :
                            args->pdata[0]);
        JSCValue *object = (args->len == 1 ? args->pdata[0] : args->pdata[1]);
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, (char *)extension_id));
        jsc_value_object_set_property(wrapper, "tabId", tab_id);
        jsc_value_object_set_property(wrapper, "css", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.insertCSS", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

static JSCValue *
tabs_remove_css_callback (GPtrArray *args, void *extension_id)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *tab_id = (args->len == 1 ?
                            jsc_value_new_number(context, 0) :
                            args->pdata[0]);
        JSCValue *object = (args->len == 1 ? args->pdata[0] : args->pdata[1]);
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, (char *)extension_id));
        jsc_value_object_set_property(wrapper, "tabId", tab_id);
        jsc_value_object_set_property(wrapper, "css", object);
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.removeCSS", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

static JSCValue *
tabs_execute_script_callback (GPtrArray *args, void *extension_id)
{
        JSCContext *context = jsc_context_get_current();
        JSCValue *tab_id = (args->len == 1 ?
                            jsc_value_new_number(context, 0) :
                            args->pdata[0]);
        JSCValue *object = (args->len == 1 ? args->pdata[0] : args->pdata[1]);
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
                jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, (char *)extension_id));
        jsc_value_object_set_property(wrapper, "tabId", tab_id);
        jsc_value_object_set_property(wrapper, "script", object);
                char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("ms", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.executeScript", variant);
        SEND_MESSAGE_RETURN_PROMISE(message, context, i);
}

void
inject_tabs_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        char *extension_id = get_extension_id(extension_name);
        MAKE_CLASS(context, Tabs, "tabs");

        MAKE_EVENT(context, "tabs", "onActivated");
        MAKE_EVENT(context, "tabs", "onActiveChanged");
        /* TODO_PROP(Tabs, onAttached); */
        MAKE_EVENT(context, "tabs", "onCreated");
        /* TODO_PROP(Tabs, onDetached); */
        /* TODO_PROP(Tabs, onHighlightChanged); */
        /* TODO_PROP(Tabs, onHighlighted); */
        /* TODO_PROP(Tabs, onMoved); */
        /* TODO_PROP(Tabs, onRemoved); */
        /* TODO_PROP(Tabs, onReplaced); */
        /* TODO_PROP(Tabs, onSelectionChanged); */
        /* TODO_PROP(Tabs, onUpdated); */
        /* TODO_PROP(Tabs, onZoomChange); */

        MAKE_FN(context, "tabs", "query", tabs_query_callback, NULL, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        MAKE_FN(context, "tabs", "create", tabs_create_callback, NULL, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        MAKE_FN(context, "tabs", "getCurrent", tabs_get_current_callback, NULL, JSC_TYPE_VALUE, 0, G_TYPE_NONE);
        MAKE_FN(context, "tabs", "get", tabs_get_callback, NULL, JSC_TYPE_VALUE, 1, JSC_TYPE_VALUE);
        MAKE_FN(context, "tabs", "print", tabs_print_callback, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);

        MAKE_FNV(context, "tabs", "sendMessage", tabs_send_message_callback, extension_id);
        MAKE_FNV(context, "tabs", "insertCSS", tabs_insert_css_callback, extension_id);
        MAKE_FNV(context, "tabs", "removeCSS", tabs_remove_css_callback, extension_id);
        MAKE_FNV(context, "tabs", "executeScript", tabs_execute_script_callback, extension_id);

        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                                      "TAB_ID_NONE", jsc_value_new_number(context, 0));
        TODO_METHOD(context, tabs, captureTab);
        TODO_METHOD(context, tabs, captureVisibleTab);
        TODO_METHOD(context, tabs, connect);
        TODO_METHOD(context, tabs, detectLanguage);
        TODO_METHOD(context, tabs, discard);
        TODO_METHOD(context, tabs, duplicate);
        TODO_METHOD(context, tabs, getAllInWindow); /* DEPRECATED */
        TODO_METHOD(context, tabs, getSelected); /* DEPRECATED */
        TODO_METHOD(context, tabs, getZoom);
        TODO_METHOD(context, tabs, getZoomSettings);
        TODO_METHOD(context, tabs, goForward);
        TODO_METHOD(context, tabs, goBack);
        TODO_METHOD(context, tabs, hide); /* EXPERIMENTAL */
        TODO_METHOD(context, tabs, highlight);
        TODO_METHOD(context, tabs, move);
        TODO_METHOD(context, tabs, moveInSuccession);
        TODO_METHOD(context, tabs, printPreview);
        TODO_METHOD(context, tabs, reload);
        TODO_METHOD(context, tabs, remove);
        TODO_METHOD(context, tabs, saveAsPDF);
        TODO_METHOD(context, tabs, sendRequest); /* DEPRECATED */
        TODO_METHOD(context, tabs, setZoom);
        TODO_METHOD(context, tabs, setZoomSettings);
        TODO_METHOD(context, tabs, show);
        TODO_METHOD(context, tabs, toggleReaderMode);
        TODO_METHOD(context, tabs, update);
        TODO_METHOD(context, tabs, warmup);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "tabs",
                jsc_context_evaluate(context, "tabs", -1));
}
