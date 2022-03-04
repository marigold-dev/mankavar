#define CAML_NAME_SPACE
#include "caml/mlvalues.h"

CAMLprim value int64_add_inplace(value a , value b , value c) {
  return Val_unit ;
}