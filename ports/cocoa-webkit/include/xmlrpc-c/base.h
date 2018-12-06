/* Copyright and license information is at the end of the file */

#ifndef XMLRPC_H_INCLUDED
#define XMLRPC_H_INCLUDED

#include <stddef.h>
#include <stdarg.h>
#include <time.h>
#include <xmlrpc-c/c_util.h>  /* For XMLRPC_DLLEXPORT */
#include <xmlrpc-c/util.h>
#include <xmlrpc-c/config.h>
  /* Defines XMLRPC_HAVE_WCHAR, XMLRPC_INT64, XMLRPC_HAVE_TIMEVAL */

#if XMLRPC_HAVE_WCHAR
#include <wchar.h>
#endif

#if XMLRPC_HAVE_TIMEVAL
#include <sys/time.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
  XMLRPC_LIB_EXPORTED marks a symbol in this file that is exported from
  libxmlrpc.

  XMLRPC_BUILDING_LIB says this compilation is part of libxmlrpc, as
  opposed to something that _uses_ libxmlrpc.
*/
#ifdef XMLRPC_BUILDING_LIB
#define XMLRPC_LIB_EXPORTED XMLRPC_DLLEXPORT
#else
#define XMLRPC_LIB_EXPORTED
#endif

/*=========================================================================
**  Global static library initialization
**=======================================================================*/

void
xmlrpc_init(xmlrpc_env * const envP);

void
xmlrpc_term(void);

/*=========================================================================
**  Version of libxmlrpc
**=======================================================================*/

/* These are for backward compatibility -- they can't be exported from a
   Windows DLL.  xmlrpc_server_version() is preferred.
*/
extern unsigned int const xmlrpc_version_major;
extern unsigned int const xmlrpc_version_minor;
extern unsigned int const xmlrpc_version_point;

XMLRPC_LIB_EXPORTED
void
xmlrpc_version(unsigned int * const majorP,
               unsigned int * const minorP,
               unsigned int * const pointP);

/*=========================================================================
**  C types equivalent to XML-RPC types
**=======================================================================*/

/*  We define names for these types, because they may change from platform
    to platform.
*/

typedef signed int xmlrpc_int;  
    /* An integer of the type defined by XML-RPC <int>; i.e. 32 bit */
typedef XMLRPC_INT32 xmlrpc_int32;
    /* An integer of the type defined by XML-RPC <i4>; i.e. 32 bit */
typedef XMLRPC_INT64 xmlrpc_int64;
    /* An integer of the type defined by "XML-RPC" <i8>; i.e. 64 bit */
typedef int xmlrpc_bool;
    /* A boolean (of the type defined by XML-RPC <boolean>, but there's
       really only one kind)
    */
typedef double xmlrpc_double;
    /* A double precision floating point number as defined by
       XML-RPC <float>.  But the C "double" type is universally the same,
       so it's probably clearer just to use that.  This typedef is here 
       for mathematical completeness.
    */
typedef struct {
    /* A datetime of the type defined by XML-RPC <dateTime.iso8601> with
       a few extensions.  I.e. in the range 1-9999 AD with microsecond
       resolution.
    */
        unsigned int Y;   /* 1-? */
        unsigned int M;   /* 1-12 */
        unsigned int D;   /* 1-31 */
        unsigned int h;   /* 0-23 */
        unsigned int m;   /* 0-59 */
        unsigned int s;   /* 0-59 */
        unsigned int u;   /* 0-999999 */
} xmlrpc_datetime;

/* xmlrpc_socket is just for backward compatibility, in case someone decided
   to use this in user code.  New code should use the native type for a
   socket (e.g. int or SOCKET).  (We stopped using this because for winsock
   users, we would have to #include <winsock.h> in every file that
   #includes <xmlrpc-c/base.h> and we don't want that).
*/
typedef int xmlrpc_socket;

#define XMLRPC_INT32_MAX 0x7fffffff
#define XMLRPC_INT32_MIN (-XMLRPC_INT32_MAX - 1)

#define XMLRPC_INT64_MAX 0x7fffffffffffffffll
#define XMLRPC_INT64_MIN (-XMLRPC_INT64_MAX - 1)


/*=========================================================================
**  xmlrpc_value
**=========================================================================
**  An XML-RPC value (of any type).
*/

typedef enum {
    XMLRPC_TYPE_INT      =  0,
    XMLRPC_TYPE_BOOL     =  1,
    XMLRPC_TYPE_DOUBLE   =  2,
    XMLRPC_TYPE_DATETIME =  3,
    XMLRPC_TYPE_STRING   =  4,
    XMLRPC_TYPE_BASE64   =  5,
    XMLRPC_TYPE_ARRAY    =  6,
    XMLRPC_TYPE_STRUCT   =  7,
    XMLRPC_TYPE_C_PTR    =  8,
    XMLRPC_TYPE_NIL      =  9,
    XMLRPC_TYPE_I8       = 10,
    XMLRPC_TYPE_DEAD     = 0xDEAD
} xmlrpc_type;

#define XMLRPC_HAVE_I8 1

typedef struct _xmlrpc_value xmlrpc_value;

XMLRPC_LIB_EXPORTED
const char *
xmlrpc_type_name(xmlrpc_type const type);

XMLRPC_LIB_EXPORTED
void
xmlrpc_abort_if_array_bad(xmlrpc_value * const arrayP);

#define XMLRPC_ASSERT_ARRAY_OK(val) \
    xmlrpc_abort_if_array_bad(val)

/* Increment the reference count of an xmlrpc_value. */
XMLRPC_LIB_EXPORTED
extern void xmlrpc_INCREF(xmlrpc_value* const value);

/* Decrement the reference count of an xmlrpc_value. If there
** are no more references, free it. */
XMLRPC_LIB_EXPORTED
extern void xmlrpc_DECREF(xmlrpc_value* const value);

/* Get the type of an XML-RPC value. */
XMLRPC_LIB_EXPORTED
extern xmlrpc_type xmlrpc_value_type (xmlrpc_value* const value);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_int_new(xmlrpc_env * const envP,
               int          const intValue);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_read_int(xmlrpc_env *         const envP,
                const xmlrpc_value * const valueP,
                int *                const intValueP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_i8_new(xmlrpc_env * const envP, 
              xmlrpc_int64 const value);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_read_i8(xmlrpc_env *         const envP,
               const xmlrpc_value * const valueP,
               xmlrpc_int64 *       const intValueP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_bool_new(xmlrpc_env * const envP,
                xmlrpc_bool  const boolValue);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_bool(xmlrpc_env *         const envP,
                 const xmlrpc_value * const valueP,
                 xmlrpc_bool *        const boolValueP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_double_new(xmlrpc_env * const envP,
                  double       const doubleValue);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_double(xmlrpc_env *         const envP,
                   const xmlrpc_value * const valueP,
                   xmlrpc_double *      const doubleValueP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_datetime_new(xmlrpc_env *    const envP, 
                    xmlrpc_datetime const dt);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_datetime_new_str(xmlrpc_env * const envP,
                        const char * const value);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_datetime_new_sec(xmlrpc_env * const envP, 
                        time_t       const value);

XMLRPC_LIB_EXPORTED
xmlrpc_value*
xmlrpc_datetime_new_usec(xmlrpc_env * const envP,
                         time_t       const secs,
                         unsigned int const usecs);

#if XMLRPC_HAVE_TIMEVAL
XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_datetime_new_timeval(xmlrpc_env *   const envP, 
                            struct timeval const value);
#endif

#if XMLRPC_HAVE_TIMESPEC
XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_datetime_new_timespec(xmlrpc_env *    const envP, 
                             struct timespec const value);
#endif

void
XMLRPC_LIB_EXPORTED
xmlrpc_read_datetime(xmlrpc_env *         const envP,
                     const xmlrpc_value * const valueP,
                     xmlrpc_datetime *    const dtP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_datetime_sec(xmlrpc_env *         const envP,
                         const xmlrpc_value * const valueP,
                         time_t *             const timeValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_datetime_usec(xmlrpc_env *         const envP,
                          const xmlrpc_value * const valueP,
                          time_t *             const secsP,
                          unsigned int *       const usecsP);

#if XMLRPC_HAVE_TIMEVAL
XMLRPC_LIB_EXPORTED
void
xmlrpc_read_datetime_timeval(xmlrpc_env *         const envP,
                             const xmlrpc_value * const valueP,
                             struct timeval *     const timeValueP);
#endif

#if XMLRPC_HAVE_TIMESPEC
XMLRPC_LIB_EXPORTED
void
xmlrpc_read_datetime_timespec(xmlrpc_env *         const envP,
                              const xmlrpc_value * const valueP,
                              struct timespec *    const timeValueP);
#endif

void
XMLRPC_LIB_EXPORTED
xmlrpc_read_datetime_8601(xmlrpc_env *         const envP,
                          const xmlrpc_value * const valueP,
                          const char **        const iso8601ValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_datetime_str(xmlrpc_env *         const envP,
                         const xmlrpc_value * const valueP,
                         const char **        const stringValueP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_new(xmlrpc_env * const envP,
                  const char * const stringValue);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_new_lp(xmlrpc_env * const envP, 
                     size_t       const length,
                     const char * const stringValue);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_new_va(xmlrpc_env * const envP,
                     const char * const format,
                     va_list            args);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_new_f(xmlrpc_env * const envP,
                    const char * const format,
                    ...);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_new_lp_cr(xmlrpc_env * const envP, 
                        size_t       const length,
                        const char * const value);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_new_cr(xmlrpc_env * const envP,
                     const char * const value);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string(xmlrpc_env *         const envP,
                   const xmlrpc_value * const valueP,
                   const char **        const stringValueP);


XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_crlf(xmlrpc_env *         const envP,
                        const xmlrpc_value * const valueP,
                        const char **        const stringValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_lp_crlf(xmlrpc_env *         const envP,
                           const xmlrpc_value * const valueP,
                           size_t *             const lengthP,
                           const char **        const stringValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_lp(xmlrpc_env *         const envP,
                      const xmlrpc_value * const valueP,
                      size_t *             const lengthP,
                      const char **        const stringValueP);

#if XMLRPC_HAVE_WCHAR
XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_w_new(xmlrpc_env *    const envP,
                    const wchar_t * const stringValue);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_w_new_lp(xmlrpc_env *    const envP, 
                       size_t          const length,
                       const wchar_t * const stringValue);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_w(xmlrpc_env *     const envP,
                     xmlrpc_value *   const valueP,
                     const wchar_t ** const stringValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_w_crlf(xmlrpc_env *     const envP,
                          xmlrpc_value *   const valueP,
                          const wchar_t ** const stringValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_w_lp(xmlrpc_env *     const envP,
                        xmlrpc_value *   const valueP,
                        size_t *         const lengthP,
                        const wchar_t ** const stringValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_string_w_lp_crlf(xmlrpc_env *     const envP,
                             xmlrpc_value *   const valueP,
                             size_t *         const lengthP,
                             const wchar_t ** const stringValueP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_w_new_lp_cr(xmlrpc_env *    const envP, 
                          size_t          const length,
                          const wchar_t * const value);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_string_w_new_cr(xmlrpc_env *    const envP,
                       const wchar_t * const value);

#endif /* XMLRPC_HAVE_WCHAR */

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_base64_new(xmlrpc_env *          const envP, 
                  size_t                const length,
                  const unsigned char * const value);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_base64(xmlrpc_env *           const envP,
                   const xmlrpc_value *   const valueP,
                   size_t *               const lengthP,
                   const unsigned char ** const bytestringValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_base64_size(xmlrpc_env *           const envP,
                        const xmlrpc_value *   const valueP,
                        size_t *               const lengthP);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_array_new(xmlrpc_env * const envP);

/* Return the number of elements in an XML-RPC array.
** Sets XMLRPC_TYPE_ERROR if 'array' is not an array. */
XMLRPC_LIB_EXPORTED
int 
xmlrpc_array_size(xmlrpc_env *         const env, 
                  const xmlrpc_value * const array);

XMLRPC_LIB_EXPORTED
void
xmlrpc_array_append_item(xmlrpc_env   * const envP,
                         xmlrpc_value * const arrayP,
                         xmlrpc_value * const valueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_array_read_item(xmlrpc_env *         const envP,
                       const xmlrpc_value * const arrayP,
                       unsigned int         const index,
                       xmlrpc_value **      const valuePP);

/* Deprecated.  Use xmlrpc_array_read_item() instead.

   Get an item from an XML-RPC array.
   Does not increment the reference count of the returned value.
   Sets XMLRPC_TYPE_ERROR if 'array' is not an array.
   Sets XMLRPC_INDEX_ERROR if 'index' is out of bounds.
*/
XMLRPC_LIB_EXPORTED
xmlrpc_value * 
xmlrpc_array_get_item(xmlrpc_env *         const envP,
                      const xmlrpc_value * const arrayP,
                      int                  const index);

/* Not implemented--we don't need it yet.
XMLRPC_LIB_EXPORTED
int
xmlrpc_array_set_item(xmlrpc_env *   const envP,
                      xmlrpc_value * const arrayP,
                      unsigned int   const index,
                      xmlrpc_value * const valueP);
*/

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_struct_new(xmlrpc_env * const env);

/* Return the number of key/value pairs in a struct.
** Sets XMLRPC_TYPE_ERROR if 'strct' is not a struct. */
XMLRPC_LIB_EXPORTED
int
xmlrpc_struct_size (xmlrpc_env   * const env, 
                    xmlrpc_value * const strct);

/* Returns true iff 'strct' contains 'key'.
** Sets XMLRPC_TYPE_ERROR if 'strct' is not a struct. */
XMLRPC_LIB_EXPORTED
int 
xmlrpc_struct_has_key(xmlrpc_env *   const envP,
                      xmlrpc_value * const strctP,
                      const char *   const key);

/* The same as the above, but the key may contain zero bytes.
   Deprecated.  xmlrpc_struct_get_value_v() is more general, and this
   case is not common enough to warrant a shortcut.
*/
XMLRPC_LIB_EXPORTED
int 
xmlrpc_struct_has_key_n(xmlrpc_env   * const envP,
                        xmlrpc_value * const strctP,
                        const char *   const key, 
                        size_t         const key_len);

#if 0
/* Not implemented yet, but needed for completeness. */
XMLRPC_LIB_EXPORTED
int
xmlrpc_struct_has_key_v(xmlrpc_env *   env, 
                        xmlrpc_value * strct,
                        xmlrpc_value * const keyval);
#endif


XMLRPC_LIB_EXPORTED
void
xmlrpc_struct_find_value(xmlrpc_env *    const envP,
                         xmlrpc_value *  const structP,
                         const char *    const key,
                         xmlrpc_value ** const valuePP);


XMLRPC_LIB_EXPORTED
void
xmlrpc_struct_find_value_v(xmlrpc_env *    const envP,
                           xmlrpc_value *  const structP,
                           xmlrpc_value *  const keyP,
                           xmlrpc_value ** const valuePP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_struct_read_value(xmlrpc_env *    const envP,
                         xmlrpc_value *  const structP,
                         const char *    const key,
                         xmlrpc_value ** const valuePP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_struct_read_value_v(xmlrpc_env *    const envP,
                           xmlrpc_value *  const structP,
                           xmlrpc_value *  const keyP,
                           xmlrpc_value ** const valuePP);

/* The "get_value" functions are deprecated.  Use the "find_value"
   and "read_value" functions instead.
*/
XMLRPC_LIB_EXPORTED
xmlrpc_value * 
xmlrpc_struct_get_value(xmlrpc_env *   const envP,
                        xmlrpc_value * const strctP,
                        const char *   const key);

/* The same as above, but the key may contain zero bytes. 
   Deprecated.  xmlrpc_struct_get_value_v() is more general, and this
   case is not common enough to warrant a shortcut.
*/
XMLRPC_LIB_EXPORTED
xmlrpc_value * 
xmlrpc_struct_get_value_n(xmlrpc_env *   const envP,
                          xmlrpc_value * const strctP,
                          const char *   const key, 
                          size_t         const key_len);

/* Set the value associated with 'key' in 'strct' to 'value'.
   Sets XMLRPC_TYPE_ERROR if 'strct' is not a struct. 
*/
XMLRPC_LIB_EXPORTED
void 
xmlrpc_struct_set_value(xmlrpc_env *   const env,
                        xmlrpc_value * const strct,
                        const char *   const key,
                        xmlrpc_value * const value);

/* The same as above, but the key may contain zero bytes.  Deprecated.
   The general way to set a structure value is xmlrpc_struct_set_value_v(),
   and this case is not common enough to deserve a shortcut.
*/
XMLRPC_LIB_EXPORTED
void 
xmlrpc_struct_set_value_n(xmlrpc_env *    const env,
                          xmlrpc_value *  const strct,
                          const char *    const key, 
                          size_t          const key_len,
                          xmlrpc_value *  const value);

/* The same as above, but the key must be an XML-RPC string.
** Fails with XMLRPC_TYPE_ERROR if 'keyval' is not a string. */
XMLRPC_LIB_EXPORTED
void 
xmlrpc_struct_set_value_v(xmlrpc_env *   const env,
                          xmlrpc_value * const strct,
                          xmlrpc_value * const keyval,
                          xmlrpc_value * const value);

/* Given a zero-based index, return the matching key and value. This
** is normally used in conjunction with xmlrpc_struct_size.
** Fails with XMLRPC_TYPE_ERROR if 'struct' is not a struct.
** Fails with XMLRPC_INDEX_ERROR if 'index' is out of bounds. */

XMLRPC_LIB_EXPORTED
void 
xmlrpc_struct_read_member(xmlrpc_env *    const envP,
                          xmlrpc_value *  const structP,
                          unsigned int    const index,
                          xmlrpc_value ** const keyvalP,
                          xmlrpc_value ** const valueP);

/* The same as above, but does not increment the reference count of the
   two values it returns, and return NULL for both if it fails, and
   takes a signed integer for the index (but fails if it is negative).

   Deprecated.  Use xmlrpc_struct_read_member() instead.
*/
XMLRPC_LIB_EXPORTED
void
xmlrpc_struct_get_key_and_value(xmlrpc_env *    const env,
                                xmlrpc_value *  const strct,
                                int             const index,
                                xmlrpc_value ** const out_keyval,
                                xmlrpc_value ** const out_value);

/* The "C pointer" type has no relation to XML-RPC.  It is here for the
   convenience of programs that use xmlrpc_value for XML-RPC purposes
   and can benefit from using it for non-XML-RPC purposes as well.

   Also, some people use libxmlrpc for xmlrpc_value alone, because sometimes
   you need to work with basic data types in richer ways than the C types
   (int, time_t, etc) allow.
*/

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_cptr_new(xmlrpc_env * const envP,
                void *       const value);

typedef void (*xmlrpc_cptr_dtor_fn)(void *, void *);

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_cptr_new_dtor(xmlrpc_env *        const envP,
                     void *              const value,
                     xmlrpc_cptr_dtor_fn const dtor,
                     void *              const dtorContext);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_cptr(xmlrpc_env *         const envP,
                 const xmlrpc_value * const valueP,
                 void **              const ptrValueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_read_nil(xmlrpc_env *   const envP,
                xmlrpc_value * const valueP);
                
XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_nil_new(xmlrpc_env * const envP);


/* Build an xmlrpc_value from a format string. */

XMLRPC_LIB_EXPORTED
xmlrpc_value * 
xmlrpc_build_value(xmlrpc_env * const env,
                   const char * const format, 
                   ...);

/* The same as the above, but using a va_list and more general */
XMLRPC_LIB_EXPORTED
void
xmlrpc_build_value_va(xmlrpc_env *    const env,
                      const char *    const format,
                      va_list         const args,
                      xmlrpc_value ** const valPP,
                      const char **   const tailP);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_decompose_value(xmlrpc_env *   const envP,
                       xmlrpc_value * const value,
                       const char *   const format, 
                       ...);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_decompose_value_va(xmlrpc_env *   const envP,
                          xmlrpc_value * const value,
                          const char *   const format,
                          va_list        const args);

/* xmlrpc_parse_value... is the same as xmlrpc_decompose_value... except
   that it doesn't do proper memory management -- it returns xmlrpc_value's
   without incrementing the reference count and returns pointers to data
   inside an xmlrpc_value structure.

   These are deprecated.  Use xmlrpc_decompose_value... instead.
*/
XMLRPC_LIB_EXPORTED
void 
xmlrpc_parse_value(xmlrpc_env *   const envP,
                   xmlrpc_value * const value,
                   const char *   const format, 
                   ...);

/* The same as the above, but using a va_list. */
XMLRPC_LIB_EXPORTED
void 
xmlrpc_parse_value_va(xmlrpc_env *   const envP,
                      xmlrpc_value * const value,
                      const char *   const format,
                      va_list        const args);

/*=========================================================================
**  Encoding XML
**=======================================================================*/

typedef enum xmlrpc_dialect {
    xmlrpc_dialect_i8,
    xmlrpc_dialect_apache
} xmlrpc_dialect;

XMLRPC_LIB_EXPORTED
void 
xmlrpc_serialize_value2(xmlrpc_env *       const envP,
                        xmlrpc_mem_block * const outputP,
                        xmlrpc_value *     const valueP,
                        xmlrpc_dialect     const dialect);

XMLRPC_LIB_EXPORTED
void
xmlrpc_serialize_value(xmlrpc_env *       const envP,
                       xmlrpc_mem_block * const outputP,
                       xmlrpc_value *     const valueP);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_serialize_params2(xmlrpc_env *       const envP,
                         xmlrpc_mem_block * const outputP,
                         xmlrpc_value *     const paramArrayP,
                         xmlrpc_dialect     const dialect);

XMLRPC_LIB_EXPORTED
void
xmlrpc_serialize_params(xmlrpc_env *       const envP,
                        xmlrpc_mem_block * const outputP,
                        xmlrpc_value *     const paramArrayP);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_serialize_call2(xmlrpc_env *       const envP,
                       xmlrpc_mem_block * const outputP,
                       const char *       const methodName,
                       xmlrpc_value *     const paramArrayP,
                       xmlrpc_dialect     const dialect);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_serialize_call(xmlrpc_env *       const envP,
                      xmlrpc_mem_block * const outputP,
                      const char *       const methodName,
                      xmlrpc_value *     const paramArrayP);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_serialize_response2(xmlrpc_env *       const envP,
                           xmlrpc_mem_block * const outputP,
                           xmlrpc_value *     const valueP,
                           xmlrpc_dialect     const dialect);

XMLRPC_LIB_EXPORTED
void
xmlrpc_serialize_response(xmlrpc_env *       const envP,
                          xmlrpc_mem_block * const outputP,
                          xmlrpc_value *     const valueP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_serialize_fault(xmlrpc_env *       const envP,
                       xmlrpc_mem_block * const outputP,
                       const xmlrpc_env * const faultP);


/*=========================================================================
**  Decoding XML
**=======================================================================*/

XMLRPC_LIB_EXPORTED
void
xmlrpc_parse_value_xml(xmlrpc_env *    const envP,
                       const char *    const xmlData,
                       size_t          const xmlDataLen,
                       xmlrpc_value ** const valuePP);

XMLRPC_LIB_EXPORTED
void 
xmlrpc_parse_call(xmlrpc_env *    const envP,
                  const char *    const xmlData,
                  size_t          const xmlDataLen,
                  const char **   const methodNameP,
                  xmlrpc_value ** const paramArrayPP);

XMLRPC_LIB_EXPORTED
void
xmlrpc_parse_response2(xmlrpc_env *    const envP,
                       const char *    const xmlData,
                       size_t          const xmlDataLen,
                       xmlrpc_value ** const resultPP,
                       int *           const faultCodeP,
                       const char **   const faultStringP);


/* xmlrpc_parse_response() is for backward compatibility */

XMLRPC_LIB_EXPORTED
xmlrpc_value *
xmlrpc_parse_response(xmlrpc_env * const envP, 
                      const char * const xmlData, 
                      size_t       const xmlDataLen);


/*=========================================================================
**  Authorization Cookie Handling
**=========================================================================
**  Routines to get and set values for authorizing via authorization
**  cookies. Both the client and server use HTTP_COOKIE_AUTH to store
**  the representation of the authorization value, which is actually
**  just a base64 hash of username:password. (This entire method is
**  a cookie replacement of basic authentication.)
**/

XMLRPC_LIB_EXPORTED
extern void xmlrpc_authcookie_set(xmlrpc_env * const env,
                                  const char * const username,
                                  const char * const password);

XMLRPC_LIB_EXPORTED
char *xmlrpc_authcookie(void);

/*=========================================================================
   Resource Limits

   Ideally, there would be enough resource limits to ensure that
   XML-RPC partners cannot cause libxmlrpc objects and routines to use
   more resource than is available for them (either by accident or
   malice).  We have a long way to go to get there.
   
=========================================================================*/
/* These functions are _not_ re-entrant and the limits are per-process
   (i.e. their values live in static global variables).
*/

/* Limit IDs. There will be more of these as time goes on. */
#define XMLRPC_NESTING_LIMIT_ID   (0)
#define XMLRPC_XML_SIZE_LIMIT_ID  (1)
#define XMLRPC_LAST_LIMIT_ID      (XMLRPC_XML_SIZE_LIMIT_ID)

/* By default, deserialized data may be no more than 64 levels deep. */
#define XMLRPC_NESTING_LIMIT_DEFAULT  (64)

/* By default, XML data from the network may be no larger than 512K.
** Some client and server modules may fail to enforce this properly. */
#define XMLRPC_XML_SIZE_LIMIT_DEFAULT (512*1024)

/* Set a specific limit to the specified value. */
XMLRPC_LIB_EXPORTED
extern void xmlrpc_limit_set (int const limit_id, size_t const value);

/* Get the value of a specified limit. */
XMLRPC_LIB_EXPORTED
extern size_t xmlrpc_limit_get (int const limit_id);


#ifdef __cplusplus
}
#endif

/* Copyright (C) 2001 by First Peer, Inc. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
** 3. The name of the author may not be used to endorse or promote products
**    derived from this software without specific prior written permission. 
**  
** THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
** SUCH DAMAGE. */

#endif

