#ifndef XMLRPC_JSON_H_INCLUDED
#define XMLRPC_JSON_H_INCLUDED

#include <xmlrpc-c/base.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
    Parse a given string as JSON and return its value as an XML-RPC value
    object.

    @param envP xmlrpc environment for error handling
    @param buf holds a pointer to a ziro terminated string
    @return the value generated or NULL (check error)
*/
xmlrpc_value *
xmlrpc_parse_json(xmlrpc_env * const envP,
                  const char * const json);


/*
    Serialize an XML-RPC value object into JSON.

    @param envP holds the xmlrpc execution environment
    @param valP holds the value to serialize
    @param out holds a mem block containing the result
*/
void
xmlrpc_serialize_json(xmlrpc_env *       const envP,
                      xmlrpc_value *     const valP,
                      xmlrpc_mem_block * const jsonP);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* XMLRPC_JSON_H_INCLUDED */
