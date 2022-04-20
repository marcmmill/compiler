/*
* File Name: scanner.c
* Compiler: MS Visual Studio 2017
* Author: Marcus Miller
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 2
* Date: 1 April 2019
* Professor: Sv. Ranev
* Purpose: Create tokens from input taken from the buffer
* Function List: aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), 
* aa_func11(), char_class(), get_next_state(), iskeyword(), scanner_init(), malar_next_token(), 
* iskeyword()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef DEBUG
/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
  	if(b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/*
* Purpose: Gets tokens from the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: void
* Return value: Returns the Token t
* Algorithm:	1: gets a character from the buffer
				2: check all special cases before sending to FSM (finite state machine), 
				if special case matches, return token
				3: if no match, sent to state machine and continues to take in characters until it
				matches a case in the transition table
				4: creates a new buffer (lex_buf) and copies the token to it
				5: finds the accepting function for the token and processes the token before returning
*/
Token malar_next_token(void) 
	{
		Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
		unsigned char c; /* input symbol */
		int state = 0; /* initial state of the FSM */
		short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
		short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
		int accept = NOAS; /* type of state - initially not accepting */

		/*DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED   */
		char tc;/*temp character used for comparing conditions*/
		int i, x;/*variables used for loops in DFA*/
		while (1) { /* endless loop broken by token returns it will generate a warning */

			c = b_getc(sc_buf);/*gets character from buffer*/


			/* Part 1: Implementation of token driven scanner */
			/* every token is possessed by its own dedicated code */
			switch (c) {
			case '=':
				if (b_getc(sc_buf) == '=') {/*if relational ==*/
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
				}
				else {
					if (b_retract(sc_buf) != RT_FAIL_1) {
						t.code = ASS_OP_T;
					}
					else
						t.code = RTE_T;
				}
				return t;
			case '.':
				if (b_mark(sc_buf, b_getcoffset(sc_buf)) == RT_FAIL_1) {
					t.code = RTE_T;
					return t;
				}
				/*The following determines if the sequence of characters forms .AND.*/
				if (b_getc(sc_buf) == 'A') {
					if (b_getc(sc_buf) == 'N') {
						if (b_getc(sc_buf) == 'D') {
							if (b_getc(sc_buf) == '.') {
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
						}
					}
				}
				/*resets back to the .*/
				if (b_reset(sc_buf) == RT_FAIL_1) {
					t.code = RTE_T;
					return t;
				}
				/*The following determines if the sequence of characters forms .OR.*/
				if (b_getc(sc_buf) == 'O') {
					if (b_getc(sc_buf) == 'R') {
						if (b_getc(sc_buf) == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							return t;
						}
					}
				}
				/*resets back to the .*/
				if (b_reset(sc_buf) == RT_FAIL_1) {
					t.code = RTE_T;
					return t;
				}
				/*if neither, it is an error*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			case '<':
				tc = b_getc(sc_buf);/*sets next char as a temp character to compare cases*/
				if (tc == '>') {/*if relational*/
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
				}
				else if (tc == '<') {/*if string concatenation*/
					t.code = SCC_OP_T;
				}
				else {/*if neither of above, it is alone and is less than*/
					if (b_retract(sc_buf) != RT_FAIL_1) {
						t.code = REL_OP_T;
						t.attribute.rel_op = LT;
					}
					else
						t.code = RTE_T;
				}
				return t;
			case '>':/*is greater than*/
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;
			case '!':/*checks for comments*/
				tc = b_getc(sc_buf);/*sets next char as a temp character to compare cases*/
				if (tc == '!') {/*if it is a comment (!!), ignore rest of line, increment line*/
					while (b_getc(sc_buf) != '\n');
					line++;
					continue;
				}
				/*if not comment, it is an error*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = tc;
				t.attribute.err_lex[2] = '\0';
				/*even if error, ignore rest of line*/
				while (b_getc(sc_buf) != '\n');
				line++;
				return t;
			case SEOF:
			case SEOF_EOF: t.code = SEOF_T; t.attribute.seof = SEOF_EOF; return t;/*if SEOF_EOF*/
			case SEOF_0: t.code = SEOF_T; t.attribute.seof = SEOF_0; return t;/*if SEOF_0*/
			case ' ':
			case 'VT':
			case 'FF':
			case '\t':
				continue;/*if any whitespace*/
			case '\r':
			case '\n':
				line++;
				continue;/*if any newline terminator*/
			case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
			case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;
			case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;
			case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;
			case '{': t.code = LBR_T; return t;/*has no attribute*/
			case '}': t.code = RBR_T; return t;/*has no attribute*/
			case '(': t.code = LPR_T; return t;/*has no attribute*/
			case ')': t.code = RPR_T; return t;/*has no attribute*/
			case ';': t.code = EOS_T; return t;/*has no attribute*/
			case ',': t.code = COM_T; return t;/*has no attribute*/

			default:/*FSM if none of special cases*/
				if (!isalpha(c) && !isalnum(c) && c != '\"') {/*if next char is ", not numerical, or not a letter, at this point it is an error*/
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = '\0';
					return t;
				}
				/* Part 2: Implementation of Finite State Machine (DFA)
						   or Transition Table driven Scanner
						   Note: Part 2 must follow Part 1 to catch the illegal symbols
				*/
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));/*sets start of lex buffer*/
				state = 0;/*sets default state to 0*/
				while (as_table[state] == NOAS) {/*runs the state machine until we find correct function*/
					state = get_next_state(state, c, &accept);/*gets next state*/
					c = b_getc(sc_buf);/*gets next character*/
				}
				if (as_table[state] == ASWR)/*if accepting state with retract, retract*/
					if (b_retract(sc_buf) == RT_FAIL_1) {
						t.code = RTE_T;
						return t;
					}
				lexend = b_getcoffset(sc_buf);/*sets the lex buffer end*/
				lex_buf = b_allocate(DEFAULT_INIT_CAPACITY, DEFAULT_INC_FACTOR, 'm');/*allocates defaults and space for lex_buf*/
				for (i = lexstart; i <= lexend; i++) {/*retract to start*/
					if (b_retract(sc_buf) == RT_FAIL_1) {
						t.code = RTE_T;
						return t;
					}
				}
				for (x = lexstart; x < lexend; x++) {/*copies contents into the buffer*/
					b_addc(lex_buf, b_getc(sc_buf));
				}
				b_addc(lex_buf, '\0');//adds null terminator to end
				t = aa_table[state](b_location(lex_buf));/*calls right function*/
				b_free(lex_buf);/*frees out lex_buf*/
				return t;
		}
	}//end while(1)
}
/*get_next_state function, done by prof*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif

       assert(next != IS);

#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}
/*
* Purpose: Gets the column index based on char c
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: char c which is the input character
* Return value: Returns the value of the column
* Algorithm: Checks each column case and returns the corresponding column if condition is met
*/
int char_class (char c){
        int val;/*the column of the transition table*/
		if (isalpha(c)) {/*if letter*/
			val = 0;
		}
		else if (c == '0') {/*if number 0*/
			val = 1;
		}
		else if (c >= '1' && c <= '9') {/*if digit between 1-9*/
			val = 2;
		}
		else if (c == '.') {/*if .*/
			val = 3;
		}
		else if (c == '@') {/*if @*/
			val = 4;
		}
		else if (c == '\"') {/*if "*/
			val = 6;
		}
		else if (c == SEOF_0 || c == SEOF_EOF) {/*if an SEOF*/
			val = 7;
		}
		else {/*if anything else*/
			val = 5;
		}
        
        return val;
}

/*
* Purpose: Accepting Function for AVID (Arithemetic Variable Identifiers) and KW (Keywords)
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the Token t
* Algorithm:	1: checks if lexeme is a keyword
				2: if not a keyword, checks if valid length and shrinks if not
				3: if valid length, returns AVID as is
*/
Token aa_func02(char lexeme[]){
	unsigned int index;/*used for loop shrinking lexeme*/
	int k_index;/*the keyword index, if -1, not a keyword*/
	Token t;/*A new token*/

	k_index = iskeyword(lexeme);/*checks for keyword*/
	if (k_index >= 0) {/*if keyword, set appropiate code and attribute*/
		t.code = KW_T;
		t.attribute.kwt_idx = k_index;
		return t;
	}
	t.code = AVID_T;/*if not keyword, it is an AVID*/
	if (strlen(lexeme) > VID_LEN) {/*if AVID is greater than allowed length, shorten it*/
		for (index = 0; index < strlen(lexeme) && index < VID_LEN; index++) {
			t.attribute.vid_lex[index] = lexeme[index];
		}
		t.attribute.vid_lex[index] = '\0';/*adds null terminator to end*/
	}
	else {/*if no need to shrink, return as is*/
		t.code = AVID_T;
		strcpy(t.attribute.vid_lex, lexeme);
	}
  return t;
}

/*
* Purpose: Accepting Function for the SVID (string variable identifier)
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the Token t
* Algorithm:	1: checks if valid length and shrinks if not
				2: if valid length, returns SVID as is
*/
Token aa_func03(char lexeme[]){
	Token t;/*A new token*/
	unsigned int index;/*used for loop shrinking lexeme*/
	t.code = SVID_T;
	if (strlen(lexeme) > VID_LEN) {/*if lexeme is longer than allowed VID length, shrink it*/
		for (index = 0; index < strlen(lexeme) && index < VID_LEN - 1; index++) {
			t.attribute.vid_lex[index] = lexeme[index];
		}
		t.attribute.vid_lex[VID_LEN - 1] = '@';
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else {/*if not longer, copy and return as is*/
		strcpy(t.attribute.vid_lex, lexeme);
	}
  return t;
}
/*
* Purpose: Accepting Function for the IL(integer literal) - DIL(decimal constant)
* Author: Marcus Miller
* Version: 1
* Called functions: aa_func11 - from aa_table to handle error token
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the Token t
* Algorithm:	1: converts lexeme into long and stores in long
				2: checks if the integer is within the correct bounds and returns if true
				3: if not true, returns token as an error
*/
Token aa_func05(char lexeme[]) {
	Token t;/*A new token*/
	long dec = atol(lexeme);/*converts the lexeme to a long, stored in a long*/
		if (dec >= SHRT_MIN && dec <= SHRT_MAX && strlen(lexeme) < INL_LEN + 1) {/*if long withing allowed range, return*/
			t.code = INL_T;
			t.attribute.int_value = dec;
		}
		else {/*if not within range, it is an error*/
			t = aa_table[ES](lexeme);/*call accepting function 11 to handle error*/
		}
		return t;
}

/*
* Purpose: Accepting Function for the FPL(floating-point literal)
* Author: Marcus Miller
* Version: 1
* Called functions: aa_func11 - from aa_table to handle error token
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the Token t
* Algorithm:	1: converts lexeme into float and stores in double to prevent overflow
				2: checks if the integer is within the correct bounds and returns if true
				3: if not true, returns token as an error
*/
Token aa_func08(char lexeme[]){
	Token t;/*A new token*/
	double dec = atof(lexeme);/*converts lexeme to a float, stored in double*/
	if ((dec >= FLT_MIN && dec <= FLT_MAX) || dec == 0) {/*if float within allowed range, retun*/
		t.code = FPL_T;
		t.attribute.flt_value = (float)dec;
	}
	else {/*if not, it is an error*/
		t = aa_table[ES](lexeme);/*call accepting function 11 to handle error*/
	}
	return t;

}


/*
* Purpose: Accepting Function for the SL(string literal)
* Author: Marcus Miller
* Version: 1
* Called functions: aa_func11 - from aa_table to handle error token
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the Token t
* Algorithm:	1: checks lexeme for newline character incase line car needs to be incremented
				2: checks for valid " on both ends of lexeme and removes them
				3: copies the string into the str_LTBL buffer and returns the token
*/
Token aa_func10(char lexeme[]){
	Token t;/*A new token*/
	unsigned int i, x;/*i used for checking for line terminator, x used to remove " if any*/
	t.attribute.str_offset = b_limit(str_LTBL);
	for (i = 0; i < strlen(lexeme); i++) {/*checks for line terminators in lexeme*/
		if (lexeme[i] == '\n') {
			line++;
		}
	}
	if (lexeme[0] == '\"' && lexeme[strlen(lexeme)-1] == '\"') {/*if first character and second last character (last is \0) are ", remove them*/
		lexeme[strlen(lexeme) - 1] = '\0';/*replace last with \0*/
		for (x = 1; x < strlen(lexeme) + 1;x++) {/*start after the " (x = 1) and add contents to string literal table*/
			b_addc(str_LTBL, lexeme[x]);
		}
		t.code = STR_T;
	}
	else {/*if not, it is an error*/
		t = aa_table[ES](lexeme);/*call accepting function 11 to handle error*/
	}
  return t;
}

/*
* Purpose: Accepting Function for the Error Tokens
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the Token t
* Algorithm:	1: creates a temp char array to hold new error message
				2: if a newline character exists, increments line var
				3: if the lexeme length is greater than the max error message length, 
				it shortens it and adds ...\0 to the end then returns the token
				4: if not, error token copied as is and returned
*/
Token aa_func11(char lexeme[]){
	Token t;/*A new token*/
	char tempError[ERR_LEN + 1] = "";/*temp char array to hold error message*/
	unsigned int i;/*used for checking for line terminator*/
	for (i = 0; i < strlen(lexeme); i++) {/*checks for line terminators in lexeme*/
		if (lexeme[i] == '\n') {
			line++;
		}
	}
	if (strlen(lexeme) > ERR_LEN) {/*if error is longer than allowed length (20), shorten it and append ...\0 to the end*/
		strncpy(tempError, lexeme, ERR_LEN - 3);
		strcat(tempError, "...\0");
		strcpy(t.attribute.err_lex, tempError);
	}
	else {/*if within bounds, copy as is and return*/
		strcpy(t.attribute.err_lex, lexeme);
	}
	t.code = ERR_T;
	return t;
}

/*
* Purpose: Function to determine if lexeme is a keyword
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: char lexeme[] which is the lexeme sent to the accepting function
* Return value: Returns the index if lexeme is keyword, -1 otherwise
* Algorithm:	1: checks each index of kw_table and compares it with the lexeme
				2: if any match the kw_table the index of the kw_table is returned
				3: if no match, -1 is returned
*/
int iskeyword(char * kw_lexeme){
	int index;/*index of the keyword table*/
	for (index = 0; index < KWT_SIZE; index++) {/*compare the lexeme with each keyword, if any match, return index*/
		if (strcmp(kw_table[index], kw_lexeme) == 0) {
			return index;
		}
	}
	return -1;/*if no match*/
}

