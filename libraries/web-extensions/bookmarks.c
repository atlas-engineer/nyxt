// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include "globals.h"
#include "bookmarks.h"

void inject_bookmarks_api (char* extension_name)
{
        JSCContext *context = get_extension_context(IS_PRIVILEGED ? NULL : extension_name);
        MAKE_CLASS(context, Bookmarks, "bookmarks");

        /* TODO_PROP(Bookmarks, onCreated); */
        /* TODO_PROP(Bookmarks, onRemoved); */
        /* TODO_PROP(Bookmarks, onChanged); */
        /* TODO_PROP(Bookmarks, onMoved); */
        /* TODO_PROP(Bookmarks, onChildrenReordered); */
        /* TODO_PROP(Bookmarks, onImportBegan); */
        /* TODO_PROP(Bookmarks, onImportEnded); */


        TODO_METHOD(context, bookmarks, create);
        TODO_METHOD(context, bookmarks, get);
        TODO_METHOD(context, bookmarks, getChildren);
        TODO_METHOD(context, bookmarks, getRecent);
        TODO_METHOD(context, bookmarks, getSubTree);
        TODO_METHOD(context, bookmarks, getTree);
        TODO_METHOD(context, bookmarks, move);
        TODO_METHOD(context, bookmarks, remove);
        TODO_METHOD(context, bookmarks, removeTree);
        TODO_METHOD(context, bookmarks, search);
        TODO_METHOD(context, bookmarks, update);

        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "bookmarks",
                jsc_context_evaluate(context, "bookmarks", -1));
}
