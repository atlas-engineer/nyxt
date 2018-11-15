/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <webkit2/webkit2.h>

typedef struct {
	WebKitWebView *web_view;
	int callBackCount;
	char *identifier;
} Buffer;

void buffer_set_url(Buffer *buffer, char *url) {
	webkit_web_view_load_uri(buffer->web_view, url);
}

Buffer *buffer_init() {
	Buffer *buffer = calloc(1, sizeof (Buffer));
	buffer->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
	buffer->callBackCount = 0;
	buffer_set_url(buffer, "https://next.atlas.engineer/");
	return buffer;
}
