// From: https://developer.gnome.org/gnome-devel-demos/unstable/custom-gsource.c.html.en
// Written By Philip Withnall.
// This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License.

#include <glib.h>

/**
 * MessageQueueSource:
 *
 * This is a #GSource which wraps a #GAsyncQueue and is dispatched whenever a
 * message can be pulled off the queue. Messages can be enqueued from any
 * thread.
 *
 * The callbacks dispatched by a #MessageQueueSource have type
 * #MessageQueueSourceFunc.
 *
 * #MessageQueueSource supports adding a #GCancellable child source which will
 * additionally dispatch if a provided #GCancellable is cancelled.
 */
typedef struct {
	GSource parent;
	GAsyncQueue *queue; /* owned */
	GDestroyNotify destroy_message;
} MessageQueueSource;

/**
 * MessageQueueSourceFunc:
 * @message: (transfer full) (nullable): message pulled off the queue
 * @user_data: user data provided to g_source_set_callback()
 *
 * Callback function type for #MessageQueueSource.
 */
typedef gboolean (*MessageQueueSourceFunc) (gpointer message,
	gpointer user_data);

static gboolean message_queue_source_prepare(GSource *source, gint *timeout_) {
	MessageQueueSource *message_queue_source = (MessageQueueSource *)source;

	return g_async_queue_length(message_queue_source->queue) > 0;
}

static gboolean message_queue_source_dispatch(GSource *source,
	GSourceFunc callback,
	gpointer user_data) {
	MessageQueueSource *message_queue_source = (MessageQueueSource *)source;
	gpointer message;
	MessageQueueSourceFunc func = (MessageQueueSourceFunc)callback;

	/* Pop a message off the queue. */
	message = g_async_queue_try_pop(message_queue_source->queue);

	/* If there was no message, bail. */
	if (message == NULL) {
		/* Keep the source around to handle the next message. */
		return TRUE;
	}

	/* @func may be %NULL if no callback was specified.
	 * If so, drop the message. */
	if (func == NULL) {
		if (message_queue_source->destroy_message != NULL) {
			message_queue_source->destroy_message(message);
		}

		/* Keep the source around to consume the next message. */
		return TRUE;
	}

	return func(message, user_data);
}

static void message_queue_source_finalize(GSource *source) {
	MessageQueueSource *message_queue_source = (MessageQueueSource *)source;

	g_async_queue_unref(message_queue_source->queue);
}

static gboolean message_queue_source_closure_callback(gpointer message,
	gpointer user_data) {
	GClosure *closure = user_data;
	GValue param_value = G_VALUE_INIT;
	GValue result_value = G_VALUE_INIT;
	gboolean retval;

	/* The invoked function is responsible for freeing @message. */
	g_value_init(&result_value, G_TYPE_BOOLEAN);
	g_value_init(&param_value, G_TYPE_POINTER);
	g_value_set_pointer(&param_value, message);

	g_closure_invoke(closure, &result_value, 1, &param_value, NULL);
	retval = g_value_get_boolean(&result_value);

	g_value_unset(&param_value);
	g_value_unset(&result_value);

	return retval;
}

static GSourceFuncs message_queue_source_funcs =
{
	message_queue_source_prepare,
	NULL, /* check */
	message_queue_source_dispatch,
	message_queue_source_finalize,
	(GSourceFunc)message_queue_source_closure_callback,
	NULL,
};

/**
 * message_queue_source_new:
 * @queue: the queue to check
 * @destroy_message: (nullable): function to free a message, or %NULL
 * @cancellable: (nullable): a #GCancellable, or %NULL
 *
 * Create a new #MessageQueueSource, a type of #GSource which dispatches for
 * each message queued to it.
 *
 * If a callback function of type #MessageQueueSourceFunc is connected to the
 * returned #GSource using g_source_set_callback(), it will be invoked for each
 * message, with the message passed as its first argument. It is responsible for
 * freeing the message. If no callback is set, messages are automatically freed
 * as they are queued.
 *
 * Returns: (transfer full): a new #MessageQueueSource
 */
GSource *message_queue_source_new(GAsyncQueue *queue,
	GDestroyNotify destroy_message,
	GCancellable *cancellable) {
	GSource *source; /* alias of @message_queue_source */
	MessageQueueSource *message_queue_source; /* alias of @source */

	g_return_val_if_fail(queue != NULL, NULL);
	g_return_val_if_fail(cancellable == NULL ||
		G_IS_CANCELLABLE(cancellable), NULL);

	source = g_source_new(&message_queue_source_funcs,
			sizeof (MessageQueueSource));
	message_queue_source = (MessageQueueSource *)source;

	/* The caller can overwrite this name with something more useful later. */
	g_source_set_name(source, "MessageQueueSource");

	message_queue_source->queue = g_async_queue_ref(queue);
	message_queue_source->destroy_message = destroy_message;

	/* Add a cancellable source. */
	if (cancellable != NULL) {
		GSource *cancellable_source;

		cancellable_source = g_cancellable_source_new(cancellable);
		g_source_set_dummy_callback(cancellable_source);
		g_source_add_child_source(source, cancellable_source);
		g_source_unref(cancellable_source);
	}

	return source;
}
