
#include "finger_radix.h"
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
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

double get_time()
{
   struct timeval t;
   struct timezone tzp;
   gettimeofday(&t, &tzp);
   return t.tv_sec + t.tv_usec*1e-6;
}

int main(void) {
	char *ss[] =
	            { "aaaaaaaaaaabaa"
                    , "access"
                    , "added"
                    , "adobe"
                    , "aedb"
                    , "agakb"
                    , "agoi"
                    , "aksessionid"
                    , "aktimeoffset"
                    , "allow"
                    , "alternate"
                    , "amazons"
                    , "apache"
                    , "apkailw"
                    , "arizona"
                    , "auth"
                    , "authority"
                    , "authorized"
                    , "autoplay"
                    , "autoplayexperi"
                    , "baltimore"
                    , "bbwu"
                    , "befa"
                    , "bfrom"
                    , "bittorrent"
                    , "bmja"
                    , "bmtfeqw"
                    , "books"
                    , "bootstrap"
                    , "bootstrapinf"
                    , "bootstrapinfoi"
                    //, "bootstrapinfoibootstrapinfoibootstrapinfoibootstrapinfoibootstrapinfoibootstrapinfoibootstrapinfoi"
                    , "ccbdbfb"
                    , "cdfw"
                    , "cdur"
                    , "certificates"
                    , "certification"
                    , "cexpire"
                    , "cgir"
                    , "cgrb"
                    , "chroma"
                    , "cinitcwndbps"
                    , "cipbits"
                    , "citag"
                    , "qwerty"
                    , "asdfg"
                    , "zxcvb"
                    , "lkjhgf"
                    , "mnbvcx"
		    , 0
                    };
	int i=0;
	int j=0;
	double before = get_time();
	for (j=0;j<500;j++)
	for (i=0;ss[i];i++) {
          char * s0 = ss[i];
          int l = strlen(s0);
	  struct str_input i0 = { .p = (uint8_t *)s0, .pe = s0+l};
	  finger_radix_trie_clb_t cb = { __has_more_input
	                               , __input
	         	               , __match
	                               };
	  int *r =  (int *)finger_radix_trie_lookup(&i0, &cb);
	  //printf("%s, %i\n",s0, r?*r:0);
	}
	double after = get_time();
	printf("trie: %f\n", after-before);
	before = get_time();
	for (j=0;j<500;j++)
	for (i=0;ss[i];i++) {
          char * s0 = ss[i];
          int l = strlen(s0);
	  void * p = __gperf_finger_lookup((char *)s0, l);
	  //printf("%p\n",p);
	}
	after = get_time();
	printf("hash: %f\n", after-before);
	return 0;
}
