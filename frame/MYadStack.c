static char MYadSid[]="$Id: MYadStack.c 3723 2011-02-24 13:34:42Z llh $";

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define USHREAL8ARRAY ushreal8array
#      define OPREAL8ARRAY opreal8array
# else
#   ifdef F2CSTYLE
#      define USHREAL8ARRAY ushreal8array_
#      define OPREAL8ARRAY opreal8array_
#   else
#      define USHREAL8ARRAY ushreal8array_
#      define OPREAL8ARRAY opreal8array_
#   endif
# endif
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MYONE_BLOCK_SIZE 16384
#ifndef MYSTACK_SIZE_TRACING
#define MYSTACK_SIZE_TRACING 1
#endif
/* The main stack is a double-chain of MYDoubleChainedBlock objects.
 * Each MYDoubleChainedBlock holds an array[MYONE_BLOCK_SIZE] of char. */
typedef struct _MYdoubleChainedBlock{
  struct _MYdoubleChainedBlock *prev ;
  char                       *contents ;
  struct _MYdoubleChainedBlock *next ;
} MYDoubleChainedBlock ;

/* Globals that define the current position in the stack: */
static MYDoubleChainedBlock *MYcurStack = NULL ;
static char               *MYcurStackTop    = NULL ;
/* Globals that define the current LOOKing position in the stack: */
static MYDoubleChainedBlock *lookStack = NULL ;

static long int MYmmctraffic = 0 ;
static long int MYmmctrafficM = 0 ;
#ifdef MYSTACK_SIZE_TRACING
long int MYbigStackSize = 0;
#endif

/* PUSHes "nbChars" consecutive chars from a location starting at address "x".
 * Resets the LOOKing position if it was active.
 * Checks that there is enough space left to hold "nbChars" chars.
 * Otherwise, allocates the necessary space. */
void ushNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax = (MYcurStack)?MYONE_BLOCK_SIZE-(MYcurStackTop-(MYcurStack->contents)):0 ;
#ifdef MYSTACK_SIZE_TRACING
  MYbigStackSize += nbChars;
#endif

  MYmmctraffic += nbChars ;
  while (MYmmctraffic >= 1000000) {
     MYmmctraffic -= 1000000 ;
     MYmmctrafficM++ ;
  }

  lookStack = NULL ;
  if (nbChars <= nbmax) {
    memcpy(MYcurStackTop,x,nbChars) ;
    MYcurStackTop+=nbChars ;
  } else {
    char *inx = x+(nbChars-nbmax) ;
    if (nbmax>0) memcpy(MYcurStackTop,inx,nbmax) ;
    while (inx>x) {
      if ((MYcurStack == NULL) || (MYcurStack->next == NULL)) {
        /* Create new block: */
	MYDoubleChainedBlock *newStack ;
	char *contents = (char*)malloc(MYONE_BLOCK_SIZE*sizeof(char)) ;
	newStack = (MYDoubleChainedBlock*)malloc(sizeof(MYDoubleChainedBlock)) ;
	if ((contents == NULL) || (newStack == NULL)) {
	  MYDoubleChainedBlock *stack = MYcurStack ;
	  int nbBlocks = (stack?-1:0) ;
	  while(stack) {
	      stack = stack->prev ;
	      nbBlocks++ ;
	  }
	  printf("Out of memory (allocated %i blocks of %i bytes)\n",
		 nbBlocks, MYONE_BLOCK_SIZE) ;
          exit(0);
	}
	if (MYcurStack != NULL) MYcurStack->next = newStack ;
	newStack->prev = MYcurStack ;
	newStack->next = NULL ;
	newStack->contents = contents ;
	MYcurStack = newStack ;
        /* new block created! */
      } else
	MYcurStack = MYcurStack->next ;
      inx -= MYONE_BLOCK_SIZE ;
      if(inx>x)
	memcpy(MYcurStack->contents,inx,MYONE_BLOCK_SIZE) ;
      else {
	unsigned int nbhead = (inx-x)+MYONE_BLOCK_SIZE ;
	MYcurStackTop = MYcurStack->contents ;
	memcpy(MYcurStackTop,x,nbhead) ;
	MYcurStackTop += nbhead ;
      }
    }
  }
}

/* POPs "nbChars" consecutive chars to a location starting at address "x".
 * Resets the LOOKing position if it was active.
 * Checks that there is enough data to fill "nbChars" chars.
 * Otherwise, pops as many blocks as necessary. */
void opNarray(char *x, unsigned int nbChars) {
  unsigned int nbmax = MYcurStackTop-(MYcurStack->contents) ;
#ifdef MYSTACK_SIZE_TRACING
  MYbigStackSize -= nbChars;
#endif
  lookStack = NULL ;
  if (nbChars <= nbmax) {
    MYcurStackTop-=nbChars ;
    memcpy(x,MYcurStackTop,nbChars);
  } else {
    char *tlx = x+nbChars ;
    if (nbmax>0) memcpy(x,MYcurStack->contents,nbmax) ;
    x+=nbmax ;
    while (x<tlx) {
      MYcurStack = MYcurStack->prev ;
      if (MYcurStack==NULL) printf("MYadStack: Popping from an empty stack!!!") ;
      if (x+MYONE_BLOCK_SIZE<tlx) {
	memcpy(x,MYcurStack->contents,MYONE_BLOCK_SIZE) ;
	x += MYONE_BLOCK_SIZE ;
      } else {
	unsigned int nbtail = tlx-x ;
	MYcurStackTop=(MYcurStack->contents)+MYONE_BLOCK_SIZE-nbtail ;
	memcpy(x,MYcurStackTop,nbtail) ;
	x = tlx ;
      }
    }
  }
}

void USHREAL8ARRAY(char *x, unsigned int *n) {
  ushNarray(x,(*n*8)) ;
}
void OPREAL8ARRAY(char *x, unsigned int *n) {
  opNarray(x,(*n*8)) ;
}
