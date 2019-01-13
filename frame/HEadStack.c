static char HEadSid[]="$Id: HEadStack.c 3723 2011-02-24 13:34:42Z llh $";

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define HUSHREAL8ARRAY hushreal8array
#      define HOPREAL8ARRAY hopreal8array
#      define HUSHINTEGER4ARRAY hushinteger4array
#      define HOPINTEGER4ARRAY hopinteger4array
#      define HUSHLOGICARRAY hushlogicarray
#      define HOPLOGICARRAY hoplogicarray
# else
#   ifdef F2CSTYLE
#      define HUSHREAL8ARRAY hushreal8array_
#      define HOPREAL8ARRAY hopreal8array_
#      define HUSHINTEGER4ARRAY hushinteger4array_
#      define HOPINTEGER4ARRAY hopinteger4array_
#      define HUSHLOGICARRAY hushlogicarray_
#      define HOPLOGICARRAY hoplogicarray_
#   else
#      define HUSHREAL8ARRAY hushreal8array_
#      define HOPREAL8ARRAY hopreal8array_
#      define HUSHINTEGER4ARRAY hushinteger4array_
#      define HOPINTEGER4ARRAY hopinteger4array_
#      define HUSHLOGICARRAY hushlogicarray_
#      define HOPLOGICARRAY hoplogicarray_
#   endif
# endif
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define HEONE_BLOCK_SIZE 16384
#ifndef HESTACK_SIZE_TRACING
#define HESTACK_SIZE_TRACING 1
#endif
/* The main stack is a double-chain of HEDoubleChainedBlock objects.
 * Each HEDoubleChainedBlock holds an array[HEONE_BLOCK_SIZE] of char. */
typedef struct _HEdoubleChainedBlock{
  struct _HEdoubleChainedBlock *prev ;
  char                       *contents ;
  struct _HEdoubleChainedBlock *next ;
} HEDoubleChainedBlock ;

/* Globals that define the current position in the stack: */
static HEDoubleChainedBlock *HEcurStack = NULL ;
static char               *HEcurStackTop    = NULL ;
/* Globals that define the current LOOKing position in the stack: */
static HEDoubleChainedBlock *lookStack = NULL ;

static long int HEmmctraffic = 0 ;
static long int HEmmctrafficM = 0 ;
#ifdef HESTACK_SIZE_TRACING
long int HEbigStackSize = 0;
#endif

/* PUSHes "nbChars" consecutive chars from a location starting at address "x".
 * Resets the LOOKing position if it was active.
 * Checks that there is enough space left to hold "nbChars" chars.
 * Otherwise, allocates the necessary space. */
void HushNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax = (HEcurStack)?HEONE_BLOCK_SIZE-(HEcurStackTop-(HEcurStack->contents)):0 ;
#ifdef HESTACK_SIZE_TRACING
  HEbigStackSize += nbChars;
#endif

  HEmmctraffic += nbChars ;
  while (HEmmctraffic >= 1000000) {
     HEmmctraffic -= 1000000 ;
     HEmmctrafficM++ ;
  }

  lookStack = NULL ;
  if (nbChars <= nbmax) {
    memcpy(HEcurStackTop,x,nbChars) ;
    HEcurStackTop+=nbChars ;
  } else {
    char *inx = x+(nbChars-nbmax) ;
    if (nbmax>0) memcpy(HEcurStackTop,inx,nbmax) ;
    while (inx>x) {
      if ((HEcurStack == NULL) || (HEcurStack->next == NULL)) {
        /* Create new block: */
	HEDoubleChainedBlock *newStack ;
	char *contents = (char*)malloc(HEONE_BLOCK_SIZE*sizeof(char)) ;
	newStack = (HEDoubleChainedBlock*)malloc(sizeof(HEDoubleChainedBlock)) ;
	if ((contents == NULL) || (newStack == NULL)) {
	  HEDoubleChainedBlock *stack = HEcurStack ;
	  int nbBlocks = (stack?-1:0) ;
	  while(stack) {
	      stack = stack->prev ;
	      nbBlocks++ ;
	  }
	  printf("Out of memory (allocated %i blocks of %i bytes)\n",
		 nbBlocks, HEONE_BLOCK_SIZE) ;
          exit(0);
	}
	if (HEcurStack != NULL) HEcurStack->next = newStack ;
	newStack->prev = HEcurStack ;
	newStack->next = NULL ;
	newStack->contents = contents ;
	HEcurStack = newStack ;
        /* new block created! */
      } else
	HEcurStack = HEcurStack->next ;
      inx -= HEONE_BLOCK_SIZE ;
      if(inx>x)
	memcpy(HEcurStack->contents,inx,HEONE_BLOCK_SIZE) ;
      else {
	unsigned int nbhead = (inx-x)+HEONE_BLOCK_SIZE ;
	HEcurStackTop = HEcurStack->contents ;
	memcpy(HEcurStackTop,x,nbhead) ;
	HEcurStackTop += nbhead ;
      }
    }
  }
}

/* POPs "nbChars" consecutive chars to a location starting at address "x".
 * Resets the LOOKing position if it was active.
 * Checks that there is enough data to fill "nbChars" chars.
 * Otherwise, pops as many blocks as necessary. */
void HopNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax = HEcurStackTop-(HEcurStack->contents) ;
#ifdef HESTACK_SIZE_TRACING
  HEbigStackSize -= nbChars;
#endif
  lookStack = NULL ;
  if (nbChars <= nbmax) {
    HEcurStackTop-=nbChars ;
    memcpy(x,HEcurStackTop,nbChars);
  } else {
    char *tlx = x+nbChars ;
    if (nbmax>0) memcpy(x,HEcurStack->contents,nbmax) ;
    x+=nbmax ;
    while (x<tlx) {
      HEcurStack = HEcurStack->prev ;
      if (HEcurStack==NULL) printf("HEadStack: Popping from an empty stack!!!") ;
      if (x+HEONE_BLOCK_SIZE<tlx) {
	memcpy(x,HEcurStack->contents,HEONE_BLOCK_SIZE) ;
	x += HEONE_BLOCK_SIZE ;
      } else {
	unsigned int nbtail = tlx-x ;
	HEcurStackTop=(HEcurStack->contents)+HEONE_BLOCK_SIZE-nbtail ;
	memcpy(x,HEcurStackTop,nbtail) ;
	x = tlx ;
      }
    }
  }
}

void HUSHREAL8ARRAY(char *x, unsigned int *n) {
  HushNarray(x,(*n*8)) ;
}

void HOPREAL8ARRAY(char *x, unsigned int *n) {
  HopNarray(x,(*n*8)) ;
}

void HUSHINTEGER4ARRAY(char *x, unsigned int *n) {
  HushNarray(x,(*n*4)) ;
}

void HOPINTEGER4ARRAY(char *x, unsigned int *n) {
  HopNarray(x,(*n*4)) ;
}

void HUSHLOGICARRAY(char *x, unsigned int *n) {
  HushNarray(x,(*n*4)) ;
}

void HOPLOGICARRAY(char *x, unsigned int *n) {
  HopNarray(x,(*n*4)) ;
}


