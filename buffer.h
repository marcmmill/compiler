/* 
* File Name: buffer.h
* Compiler: MS Visual Studio 2017
* Author: Marcus Miller
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 1
* Date: 3 Feburary 2019
* Professor: Sv. Ranev
* Purpose: The header file for our buffer descriptor. Contains
* preprocessor directives and type declarations needed for our buffer.
* Function List: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), 
* b_capacity(), b_mark(), b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), 
* b_eob(), b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(), 
* b_rewind(), b_location() 
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1         /* operation failure return value 1 */
#define RT_FAIL_2 -2         /* operation failure return value 2 */
#define LOAD_FAIL -2         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */
#define GOOD 1								/* good return value */
#define FIXED 0								/* fixed mode */
#define MULTIPLICATIVE -1					/* multiplicative mode */
#define ADDITIVE 1							/* additive mode */
#define MAXIMUM_POSITIVE_VALUE SHRT_MAX-1	/* maximum value allowed for the buffer */

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFFC	/* The default flags for the buffer */
#define SET_EOB  0x0002			/* set value for the eob flag */
#define RESET_EOB 0xFFFD		/* reset value for the eob flag */
#define CHECK_EOB 0x0002		/* check value for the eob flag */
#define SET_R_FLAG 0x0001		/* set value for the r flag */
#define RESET_R_FLAG 0xFFFE		/* reset value for the r flag */
#define CHECK_R_FLAG 0x0001		/* check value for the r flag */

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);	/* Creates the buffer descriptor */
pBuffer b_addc(pBuffer const pBD, char symbol);							/* Adds a char to the buffer, expands if needed */
int b_clear(Buffer * const pBD);										/* Clears the buffer to look empty */
void b_free(Buffer * const pBD);										/* Free memory of the buffer */
int b_isfull(Buffer * const pBD);										/* Checks to see if the buffer is full */
short b_limit(Buffer * const pBD);										/* Gets the current size of th buffer */
short b_capacity(Buffer * const pBD);									/* Gets the capacity of the buffer */
short b_mark(pBuffer const pBD, short mark);							/* Gets the mark location of the buffer */
int b_mode(Buffer * const pBD);											/* Gets the operational mode of the buffer */
size_t b_incfactor(Buffer * const pBD);									/* Gets the inc_factor of the buffer */
int b_load(FILE * const fi, Buffer * const pBD);						/* Loads a file into the buffer */
int b_isempty(Buffer * const pBD);										/* Checks to see if the buffer is empty */
char b_getc(Buffer * const pBD);										/* Gets the char from the buffer */
int b_eob(Buffer * const pBD);											/* Checks the eob flag to see if the end of the buffer has been reached */
int b_print(Buffer * const pBD);										/* Print the contents of the buffer */
Buffer * b_compact(Buffer * const pBD, char symbol);					/* Resizes the buffer to be either smaller or bigger and +1 */
char b_rflag(Buffer * const pBD);										/* Checks the r flag to see if its set */
short b_retract(Buffer * const pBD);									/* Decrements getc_offset */
short b_reset(Buffer * const pBD);										/* Sets getc_offset to the markc_offset */
short b_getcoffset(Buffer * const pBD);									/* Gets the getc_offset of the buffer */
int b_rewind(Buffer * const pBD);										/* Sets getc and markc offset to 0 so the buffer can be reread */
char * b_location(Buffer * const pBD);									/* Gets the location of the markc_offset in the character buffer */

/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/

#endif

