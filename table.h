/*
* File Name: table.h
* Compiler: MS Visual Studio 2017
* Author: Marcus Miller
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 2
* Date: 1 April 2019
* Professor: Sv. Ranev
* Purpose: The file which creates the corresponding Transition Table for the state machine
* Function List: aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), 
* aa_func11()
 */

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */
#define SEOF 255
/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF, 'illegal symbol',
 */
 

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
/*Column order: [a-zA-Z], 0, [1-9], ., @, other, "", SEOF*/
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */  {1,6,4,ES,ES,ES,9,IS},
	/* State 1 */  {1,1,1,2,3,2,2,ER},
	/* State 2 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 3 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 4 */  {ES,4,4,7,5,5,5,ER},
	/* State 5 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 6 */  {ES,6,ES,7,5,5,ES,ER},
	/* State 7 */  {8,7,7,8,8,8,8,ER},
	/* State 8 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 9 */  {9,9,9,9,9,9,10,ES},
	/* State 10 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 11 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 12 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 13 */  {IS,IS,IS,IS,IS,IS,IS,IS},


};
 
/* Accepting state table definition */
#define ASWR     -12  /* accepting state with retract */
#define ASNR     -11  /* accepting state with no retract */
#define NOAS     -9  /* not accepting state */

int as_table[ ] = {NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASNR, ASNR, ASWR, 0};

/* Accepting action function declarations */
Token aa_func02(char *lexeme);/*AVID/KW accepting function*/
Token aa_func03(char *lexeme);/*SVID accepting function*/
Token aa_func05(char *lexeme);/*IL - DIL accepting function*/
Token aa_func08(char *lexeme);/*FPL accepting function*/
Token aa_func10(char *lexeme);/*SL accepting function*/
Token aa_func11(char *lexeme);/*Error token accepting function*/

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[ ] ={
NULL,
NULL,
aa_func02,
aa_func03,
NULL,
aa_func05,
NULL,
NULL,
aa_func08,
NULL,
aa_func10,
aa_func11,
aa_func11,
NULL
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table []=
	{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"   
	};

#endif
                     