/*
* File Name: parser.c
* Compiler: MS Visual Studio 2017
* Author: Marcus Miller
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 3
* Date: 19 April 2019
* Professor: Sv. Ranev
* Purpose: Header file of the parser, holds declarations for functions and variables
* Function List: parser(), match(), syn_eh(), syn_printe(), gen_incode(), Multiple production functions
*/
#include "buffer.h"
#include "token.h"

/*Tokens*/
#define NO_ATTR -1 /*No attribute */
#define ELSE 0 /*ELSE KW*/
#define FALSE 1 /*FALSE KW*/
#define IF 2 /*IF KW*/
#define PLATYPUS 3 /*PLATYPUS KW*/
#define READ 4 /*READ KW*/
#define REPEAT 5 /*REPEAT KW*/
#define THEN 6 /*THEN KW*/
#define TRUE 7 /*TRUE KW*/
#define WHILE 8 /*WHILE KW*/
#define WRITE 9 /*WRITE*/

/*Variable declarations*/
static Token lookahead;/*lookahead token used throughout parser to look ahead*/
int synerrno;/*keeps track of errors as counter*/
extern Token malar_next_token(void);/*advances lookahead, from scanner*/
extern char* kw_table[];/*The kw_table from scanner*/
extern int line;/*Keeps track of current line, from scanner*/
extern Buffer * str_LTBL; /*String literal table, from scanner */

/*Function declarations*/

void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* ic);
/*Production functions*/

void program(void);
void opt_statements(void);
void statements(void);
void statements_prime(void);
void statement(void);
/*Statement functions*/

void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void pre_condition(void);
void iteration_statement(void);
void input_statement(void);
void output_statement(void);
void opt_variable_list(void);
void output_list(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_prime(void);
/*Expression functions*/

void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);
/*String expression functions*/

void primary_string_expression(void);
void string_expression(void);
void string_expression_prime(void);
/*Conditional expression functions*/

void logical_OR_expression_prime(void);
void logical_AND_expression_prime(void);
void logical_OR_expression(void);
void logical_AND_expression(void);
void conditional_expression(void);
/*Relational expression functions*/

void primary_s_relational_expression(void);
void primary_a_relational_expression(void);
void relational_operators(void);
void relational_expression(void);

