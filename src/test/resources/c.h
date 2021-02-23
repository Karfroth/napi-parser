// asdf
#ifndef asdf
// a comment
#ifndef ifndef
 #ifdef ifdef
    #define DEFINE __somethig(a)
  #elif __something2(b)
    #define DEFINE __something3(c)                \
                   __something4(d)
  #else
    #define DEFINE __something5(e)
  #endif
#endif
NAPI_EXTERN some_type napi_func(napi_env env);

#endif

typedef struct {
  a aa;
  b bb;
  c cc;
} napi_abc;
typedef struct napi_aaa__* napi_aaa;
typedef struct {
  a aa;
  b * bb;
  c cc;
} napi_abc2;

typedef void (*napi_name)(napi_a a,
                          void* b);

typedef int32_t* (*napi_name2)(napi_a* a2,
                           void** b);