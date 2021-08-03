#include "globals.h"
#include "runtime.h"

Runtime *RUNTIME;

static void
runtime_send_message_reply_callback (GObject *web_page,
                                     GAsyncResult *res,
                                     gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "null";
        RUNTIME->reply = (char *) json;
}

static JSCValue *
runtime_send_message_result_callback ()
{
        JSCContext *context = jsc_context_get_current();
        return jsc_value_new_from_json(context, RUNTIME->reply);
}

static void
runtime_send_message_callback (char *extension_id, JSCValue *object)
{
        JSCContext *context = jsc_context_get_current();
        g_print("runtime.sendMessage entered\n");
        JSCValue *wrapper = jsc_value_new_object(context, NULL, NULL);
        g_print("Wrapper object created\n");
        jsc_value_object_set_property(
                wrapper, "extensionId",
                jsc_value_new_string(context, extension_id));
        g_print("Extension ID set\n");
        jsc_value_object_set_property(wrapper, "message", object);
        g_print("Message set\n");
        char *json = jsc_value_to_json(wrapper, 0);
        GVariant *variant = g_variant_new("s", json);
        g_print("GVariant created\n");
        WebKitUserMessage *message = webkit_user_message_new("runtime.sendMessage", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, runtime_send_message_reply_callback, NULL);
        g_print("Message sent\n");
}

void inject_runtime_api (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        MAKE_CLASS(context, Runtime, "runtime");
        JSCValue *runtimeSendMessage = jsc_value_new_function(
                context, "runtimeSendMessage",
                G_CALLBACK(runtime_send_message_callback), NULL, NULL,
                G_TYPE_NONE, 2, G_TYPE_STRING, JSC_TYPE_VALUE);
        JSCValue *runtimeSendMessageResult = jsc_value_new_function(
                context, "runtimeSendMessageResult",
                G_CALLBACK(runtime_send_message_result_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        jsc_context_set_value(context, "runtimeSendMessage", runtimeSendMessage);
        jsc_context_set_value(context, "runtimeSendMessageResult", runtimeSendMessageResult);
        char *runtime_send_message_js = "runtime.sendMessage = function (one, two, three) {\
    var no_two = (two === undefined || two === null ||                  \
                  (two.hasOwnProperty(\"includeTlsChannelId\") &&       \
                   two.keys.length <= 1));                              \
    var no_three = (three === undefined);                               \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            management.getSelf().then(function (info) {                 \
                var message = (no_two && no_three) ? one : two;         \
                var extensionId = (no_two && no_three) ? info.id : one; \
                runtimeSendMessage(extensionId, message);    \
                setTimeout(() => {                                      \
                    success(runtimeSendMessageResult());},     \
                           0);});                                       \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
runtime.sendMessage";
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "runtime", -1),
                "sendMessage",
                jsc_context_evaluate(context, runtime_send_message_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "runtime",
                jsc_context_evaluate(context, "runtime", -1));
}
