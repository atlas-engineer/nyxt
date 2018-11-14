/*
Copyright © 2018 Atlas Engineer LLC.
Use of this file is governed by the license that can be found in LICENSE.
*/

#include <glib.h>

typedef struct {
	// TODO: Why is one prefixed with an underscore and not the other?
	GHashTable *_dict;
	guint element_count;
} AutokeyDictionary;

AutokeyDictionary *akd_init(AutokeyDictionary *self) {
	if (self == NULL) {
		self = calloc(1, sizeof (AutokeyDictionary));
	}
	self->element_count = 0;
	self->_dict = g_hash_table_new(NULL, NULL);
}

void akd_free(AutokeyDictionary *self) {
	g_hash_table_unref(self->_dict);
}

char *akd_insert_element(AutokeyDictionary *self, gpointer object) {
	// TODO: Why indexing by string and not by number?
	char *element_key = g_strdup_printf("%i", self->element_count);
	g_hash_table_insert(self->_dict, element_key, object);
	self->element_count++;
	return element_key;
}

// TODO: Not needed?
guint akd_count(AutokeyDictionary *self) {
	return g_hash_table_size(self->_dict);
}

gpointer akd_object_for_key(AutokeyDictionary *self, char *a_key) {
	return g_hash_table_lookup(self->_dict, a_key);
}

void akd_remove_object_for_key(AutokeyDictionary *self, char *a_key) {
	g_hash_table_remove(self->_dict, a_key);
}

// TODO: Include "keyEnumerator"?

// TODO: Not needed?
GList *akd_all_keys(AutokeyDictionary *self) {
	g_hash_table_get_keys(self->_dict);
}
