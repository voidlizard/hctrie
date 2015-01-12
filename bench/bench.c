
#include "finger_radix.h"
#include <stdio.h>
#include "gperf_lookup.c"

struct str_input {
	uint8_t *p;
	uint8_t *pe;
};

static uint8_t __input(void *cc) {
	struct str_input *inp = cc;
	if( inp->p < inp->pe ) {
		return *(inp->p++);
	}
	return 0;
}


static int __has_more_input(void *cc){
	struct str_input *inp = cc;
	return (inp->p < inp->pe);
}

void *  __match( void *cc
		, void *r
		, int exact 
		, int consumed ) {
        struct str_input *inp = cc;
	if (exact) return r;
	return 0;
}

int main(void) {
	char *s0 = "aabaa";//"0603010f42430d2c0c2b3a3b2a02";
	struct str_input i0 = { .p = (uint8_t *)s0, .pe = s0+sizeof(s0)};
	finger_radix_trie_clb_t cb = { __has_more_input
	                             , __input
	       	                     , __match
	                             };
	int * r = (int *)finger_radix_trie_lookup(&i0, &cb);
	printf("%i\n",*r);
	void * p = __gperf_finger_lookup((char *)s0, sizeof(s0));
	printf("%p\n",p);
	return 0;
}
