#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define LLI long long int
#define OF_OINT(X) ((X) / 2)
#define TO_OINT(X) ((X) * 2 + 1)

int int_free(LLI ptr) {
  free((void *)(OF_OINT(ptr))) ;
  return 0 ;
}
int int_free_byte(LLI ptr) {
  return int_free(ptr) ;
}
LLI int_malloc(LLI size) {
  size = OF_OINT(size) ;
  void* ptr = malloc(size) ;
  *((LLI*)ptr) = (LLI)ptr ;
  printf("Alloc-ing (%Ld):\t%Ld\n" , size , (LLI)ptr) ;
  return TO_OINT((LLI)ptr) ;
}
LLI int_malloc_byte(LLI size) {
  return int_malloc(size) ;
}

LLI int_get(LLI ptr) {
  return TO_OINT(*((LLI*)(OF_OINT(ptr)))) ;
}
LLI int_get_byte(LLI ptr) {
  return int_get(ptr) ;
}

LLI int64_get(LLI ptr) {
  return *((LLI*)(OF_OINT(ptr))) ;
}
LLI int64_get_byte(LLI ptr) {
  return int64_get(ptr) ;
}
