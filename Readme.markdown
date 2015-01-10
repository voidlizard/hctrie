hctrie - static prefix trie generator
=====================================

hctrie is a tool for creating a trie that generates
a packed key-value store and functions for performing
queries on that store. The store is packed in a [Trie](http://en.wikipedia.org/wiki/Trie).

# Installation

1. Download sources
2. cd hctrie
3. cabal install

If you prefer to install library in a sandbox, i.e. don't
keep dependencies user or system wide you mind calling
`cabal sandbox init` before.

For more options on installation see [cabal guide](https://www.haskell.org/cabal/users-guide/).

# Usage

hctrie accepts a data files as an input, or read from stdin.

## File format
File has following format, it's a comma separated file, 

    key,value1,value2...

Key is a string that will be converted to a binary representation
following escaping is supported
  \DDD - escaped byte, where D stands for decimal character
  \xHH - escaped byte, where H stands for hexadecimal character
all other characters will be read as ASCI characters/

Values can be either a decimal integer or a string, if string
contains any special charactes, like non printable or separator (',')
then it should be doublequoted, in this case string will be passed
to a C program as a constant string

Example:

    \x01\x0f\x03\x06\x2c\x2e\x2f\x1f\x21\xf9\x2b,1,"Windows",100,"Microsoft Windows..."
    \x01\x0f\x03\x06\x2c\x2e\x2f\x1f\x21\xf9\x2b\xfc,1,"Windows",100,"Microsoft Windows..."
    \x01\x0f\x03\x06\x2c\x2e\x2f\x1f\x21\xf9\x2b\xfc\x0c,1,"Windows",100,"Microsoft Windows..."
    \x3c\x2b,2,"Macintosh",200,"Mac OS X"

# Calling hctrie
When program is invoked following options are possible:

    * `--prefix` add prefix to a file and generated functions
    * `--headers` provide a list of additional headers that are required for compilation
    * `--struct`  type name of the struct that is used (in case if value is not scalar)

Example of a makefile rule:

    dhcp_fp_radix.c: $(DATA)/dhcp_fingerprints.conf
      $(HCTRIE) --prefix dhcp_fp \
                --headers "\"dhcp_fp.h\""\
                --struct "struct dhcp_fp"


# API

hctrie provide a function `radix_trie_lookup(void *user_data, radix_trie_clb_t *callback)`,
this function can be used in order to find a value by key. `user_data` is any data that
is transparent for hctrie.

```
typedef struct dhcp_fp_radix_trie_clb{
  int (*has_more_input) (void *);
  uint8_t (*get_input) (void *);
  void * (*match) ( void * user_data
                  , struct dhcp_fp * data
                  , int exact
                  , int consumed);
} dhcp_fp_radix_trie_clb_t;
```

  * `has_more_input` gives information is any input is available
  * `get_input` provides next byte
  * `match` is called once a values is found.

If value was not found then the closest value will be returned and `exact`
flag will be set to `0`.

Example of usage:

```
#include <stdio.h>
#include "dhcp_fp_radix.h"
struct str_input {
  uint8_t *p;
  uint8_t *pe;
};
static uint8_t __input(void *cc) {
  struct str_input *inp = cc;
  if( inp->p < inp->pe ) {
    fprintf(stderr, "input: %c\n", *inp->p);
    return *(inp->p++);
  }
  return 0;
}
static int __has_more_input(void *cc){
  struct str_input *inp = cc;
  fprintf(stderr, "__hash_more_input\n");
  return (inp->p < inp->pe);
}
static int __match( void *cc
                  , struct dhcp_fp *r
                  , int consumed
                 , int exact ) {
  fprintf(stderr, "__match %s\n", r->class_name);
  return 1;
}
int main(void) {
  uint8_t s0[] = {1,28,2,3,15,6,119,12};
  struct str_input i0 = { .p = s0, .pe = sizeof(s0) + 1 };
  dhcp_fp_radix_trie_clb_t cb = { __has_more_input
                                , __input
                                , __match
                                };
  int r = dhcp_fp_radix_trie(&i0, &cb);
  return 0;
}
```

# Bugs

All bugs could be sent on GitHub issue tracker.
