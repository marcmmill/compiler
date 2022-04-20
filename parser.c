/*
* File Name: parser.c
* Compiler: MS Visual Studio 2017
* Author: Marcus Miller
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 3
* Date: 19 April 2019
* Professor: Sv. Ranev
* Purpose: Parse tokens taken from the scanner
* Function List: parser(), match(), syn_eh(), syn_printe(), gen_incode(), Multiple production functions
*/
#include "parser.h"
/*
* Purpose: Starts the parsing process
* Author: Marcus Miller
* Version: 1
* Called functions: malar_next_token(), program(), match(), gen_incode()
* Parameters: void
* Return value: N/A
* Algorithm:	1: sets lookahead to next token
				2: calls program() function
				3: match() called for seof upon end of program
				
*/
void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
/*
* Purpose: matches tokens with attributes from parser
* Author: Marcus Miller
* Version: 1
* Called functions: syn_eh(), malar_next_token(), syn_printe()
* Parameters:	int pr_token_code = token taken from parser
				int pr_token_attribute = token attribute from parser
* Return value: N/A
* Algorithm:	1: checks lookahead.code for a token from the parser
				2: Checks each variation of Tokens to see if their attribute matches up, if not, syn_eh() called
				3: Checks for SEOF
				4: Advances lookahead to next token if not SEOF
				5: Checks if next token is an error token
*/
void match(int pr_token_code, int pr_token_attribute){
	
	if (lookahead.code == pr_token_code) {
		
		switch (pr_token_code) {
		case KW_T:
			if (lookahead.attribute.kwt_idx != pr_token_attribute) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case LOG_OP_T:
			if (lookahead.attribute.log_op != pr_token_attribute) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case ART_OP_T:
			if (lookahead.attribute.arr_op != pr_token_attribute) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case REL_OP_T:
			if (lookahead.attribute.rel_op != pr_token_attribute) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		default:
			break;
		}
		if (lookahead.code == SEOF_T) {
			return;
		}

		lookahead = malar_next_token();
		
		if (lookahead.code == ERR_T) {
			syn_printe();
			lookahead = malar_next_token();
			synerrno++;
			return;
		}
	}
	else {
		syn_eh(pr_token_code);
		return;
	}
}
/*
* Purpose: Emergency error handler function
* Author: Marcus Miller
* Version: 1
* Called functions: malar_next_token(), syn_printe(), exit()
* Parameters: int sync_token_code = token taken by parser to sync with lookahead
* Return value: N/A
* Algorithm:	1: sets lookahead to next token
				2: calls program() function
				3: match() called for seof upon end of program

*/
void syn_eh(int sync_token_code) {
	syn_printe();//calls syn_printe() to print error
	synerrno++;//increment synerrno
	while (lookahead.code != sync_token_code) {
		if (sync_token_code != SEOF_T && (lookahead.code == SEOF_T)) {//reached end of file before sync_token_code found
			exit(synerrno);
		}
		lookahead = malar_next_token();//advance
	}//matching token found
	if (sync_token_code != SEOF_T) {//if not SEOF_T
		lookahead = malar_next_token();//advance
	}
	return;
}

/* error printing function for Assignment 3 (Parser), W19 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*
* Purpose: Prints string declaring if function/program was parsed
* Author: Marcus Miller
* Version: 1
* Called functions: printf()
* Parameters: char * ic = the incode to be printed
* Return value: N/A
* Algorithm:

*/
void gen_incode(char* ic) {
	printf("%s\n", ic);
}
/*<program>->PLATYPUS{<opt_statements>}
FIRST(<program>) = {KW_T(PLATYPUS)}*/
void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T,NO_ATTR);
	gen_incode("PLATY: Program parsed");
}
/*<opt_statements>-> <statements> | e
FIRST(<opt_statements>) = {FIRST(<statements>), e}
 = {AVID,SVID,KW_T(IF),KW_T(WHILE),KW_T(READ),KW_T(WRITE), e}
*/
void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF || lookahead.attribute.get_int == WHILE || lookahead.attribute.get_int == READ || lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
	default:
		gen_incode("PLATY: Opt_statements parsed");
		break;
	}
}
/*<statements>-><statement><statements’>
FIRST(<statements>) = {FIRST(<statement>)} = {AVID,SVID,KW_T(IF),KW_T(WHILE),KW_T(READ),KW_T(WRITE)}
*/
void statements(void) {
	statement();
	statements_prime();
}
/*<statements’>-><statement><statements’> | e
FIRST(<statements’>) = {FIRST(<statement>), e} = {AVID,SVID,KW_T(IF),KW_T(WHILE),KW_T(READ),KW_T(WRITE), e}*/
void statements_prime(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); statements_prime(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF || lookahead.attribute.get_int == WHILE || lookahead.attribute.get_int == READ || lookahead.attribute.get_int == WRITE) {
			statement(); 
			statements_prime();
			break;
		}
	default:
		break;
	}
}
/*<statement>-><assign statement>|<select statement >|<iteration statement> | <input statement >|<output statement>
FIRST(<statement>) = { FIRST(<assign statement>), FIRST(< select statement>), FIRST(<iteration statement>), FIRST(<input statement>), FIRST(<output statement>)} = {AVID,SVID,KW_T(IF),KW_T(WHILE),KW_T(READ),KW_T(WRITE)}
*/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF) {
			selection_statement();
		}
		else if (lookahead.attribute.get_int == WHILE) {
			iteration_statement();
		}
		else if (lookahead.attribute.get_int == READ) {
			input_statement();
		}
		else if (lookahead.attribute.get_int == WRITE) {
			output_statement();
		}
		break;
	default:
		syn_printe();
	}
}
//statements
/*<assign statement>-><assign expression>
FIRST(<assign statement>) = {FIRST(<assign expression>)} = {AVID,SVID}*/
void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}
/*<assign expression>-> AVID = <arithmetic expression> | SVID = <string expression>
FIRST(<assign expression>) = {AVID,SVID}*/
void assignment_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: 
		syn_printe();
		break;
	}
}
/*<select statement > -> KW_T(IF) <pre condition>(<conditional expression>) KW_T(THEN) {<opt_statements>} KW_T(ELSE) {<opt_statements>};
FIRST(<select statement>) = {KW_T(IF)}*/
void selection_statement(void) {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}
/*<pre-condition>-> KW_T(TRUE) | KW_T(FALSE)
FIRST(<pre condition>) = {KW_T(TRUE), KW_T(FALSE)}*/
void pre_condition(void) {
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.attribute.kwt_idx == TRUE) {
			match(KW_T, TRUE);
			break;
		}
		else if (lookahead.attribute.kwt_idx == FALSE) {
			match(KW_T, FALSE);
			break;
		}
		else {
			syn_printe();
			break;
		}
	default:
		syn_printe();
		break;
	}
}
/* 
<iteration statement> -> KW_T(WHILE)<pre-condition>(<conditional expression>) KW_T(REPEAT){<statements>};
FIRST(<iteration statement>), FIRST(<input statement>), FIRST(<output statement>)} = {AVID,SVID,KW_T(IF),KW_T(WHILE),KW_T(READ),KW_T(WRITE)}
*/
void iteration_statement(void) {
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");

}
/*<input statement> -> KW_T(READ) (<variable list>);
FIRST(<input statement>) = {KW_T(READ)}*/
void input_statement(void) {
	match(KW_T, READ); match(LPR_T, NO_ATTR); variable_list();
	match(RPR_T, NO_ATTR);  match(EOS_T,NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
/*<output statement>->KW_T(WRITE)(<output list>);
FIRST(<output statement>) = {KW_T(WRITE)}*/
void output_statement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");

}
/*<opt_variable list>-> <variable list> | e
FIRST(<opt_variable list>) = {FIRST(<variable list>), e} = {AVID_T,SVID_T,e}*/
void opt_variable_list(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		variable_list();
		break;
	default:
		break;
	}
}
/*<output list>-><opt_variable list> | STR_T
FIRST(<output list>) = {FIRST(opt_variable list), STR_T} = {AVID_T,SVID_T,e,STR_T}*/
void output_list(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		opt_variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}
/*<variable list>-><variable identifier><variable list’>
FIRST(<variable list>) = {FIRST(<variable identifier>)} = {AVID_T,SVID_T}*/
void variable_list(void) {
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}
/*<variable identifier>->AVID_T | SVID_T
FIRST(<variable identifier>) = {AVID_T, SVID_T}*/
void variable_identifier(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}
/*<variable list’>->,<variable identifier><variable list’> | e
FIRST(<variable list’>) = {FIRST(<variable identifier>),e} = {COM_T,e}*/
void variable_list_prime(void) {
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
	}
}
//expressions
/*<arithmetic expression>-><unary arithmetic expression> | <additive arithmetic expression>
FIRST(<arithmetic expression>) = {FIRST(<unary arithmetic expression>), FIRST(<additive arithmetic expression>)} = {AVID_T,FPL_T,INL_T,(,+,-}
*/
void arithmetic_expression(void) {
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case (ART_OP_T):
		if ((lookahead.attribute.arr_op == PLUS) || (lookahead.attribute.arr_op == MINUS)) {
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		}
	default:
		syn_printe();
		break;
	}
}
/*<unary arithmetic expression> -> +<primary arithmetic expression> | -<primary arithmetic expression>
FIRST(<unary arithmetic expression>) = {+,-}*/
void unary_arithmetic_expression(void) {
	if (lookahead.attribute.arr_op == PLUS) {
		match(ART_OP_T, PLUS);
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
	}
	else if (lookahead.attribute.arr_op == MINUS) {
		match(ART_OP_T, MINUS);
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
	}
	else
		syn_printe();
}
/*<additive arithmetic expression>-><multiplicative arithmetic expression><additive arithmetic expression’>
FIRST(<additive arithmetic expression>) = {FIRST(<multiplicative arithmetic expression>)}= {AVID_T, FPL_T, INL_T, (}

*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();

}
/*<additive arithmetic expression’>->+<multiplicative arithmetic expression><additive arithmetic expression’> | -<multiplicative arithmetic expression><additive arithmetic expression’> | e
FIRST(<additive arithmetic expression’>)= {+,-,e}
*/
void additive_arithmetic_expression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) {
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
		else if (lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	default:
		break;
	}
}
/*<multiplicative arithmetic expression>-><primary arithmetic expression><multiplicative arithmetic expression’>
FIRST(<multiplicative arithmetic expression>) = {FIRST(<primary arithmetic expression>)}= {AVID_T,FPL_T,INL_T,( }
*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();

}
/*<multiplicative arithmetic expression’>->*<primary arithmetic expression><multiplicative arithmetic expression’> | /<primary arithmetic expression><multiplicative arithmetic expression’> | e
FIRST(<multiplicative arithmetic expression’>) - {/, *, e}*/
void multiplicative_arithmetic_expression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT) {
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");

			break;
		}
		else if (lookahead.attribute.arr_op == DIV) {
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");

			break;
		}

	default:
		break;
	}
}
/*<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T}*/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T,NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}
//string expressions
/*<primary string expression>-> SVID_T | STR_T
FIRST(<primary string expression>) = {SVID_T,STR_T}*/
void primary_string_expression(void) {
	switch (lookahead.code) {
	case SVID_T:
		match(SVID_T, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}
/*<string expression>-><primary string expression><string expression’>
FIRST(<string expression>) = {FIRST(<primary string expression>), FIRST(<string expression’>)} = {SVID_T,STR_T,<<,e}*/
void string_expression(void) {
	primary_string_expression();
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}
/*<string expression’>-> << <primary string expression><string expression’> | e
FIRST(<string expression’>) = {<<, e}*/
void string_expression_prime(void) {
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;
	default:
		break;
	}
}
//conditional expressions
/*<logical OR expression’>->.OR. <logical AND expression><logical OR expression’> | e
FIRST(<logical OR expression’>) = {.OR., e}*/
void logical_OR_expression_prime(void) {
	switch (lookahead.code)
	{
	case LOG_OP_T:
		if (lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
		}
		break;
	default:
		break;
	}
}
/* <logical AND expression’>->.AND.<relational expression><logical AND expression’> | e
FIRST(<logical AND expression’>) = {.AND., e}*/
void logical_AND_expression_prime(void) {
	switch (lookahead.code)
	{
	case LOG_OP_T:
		if (lookahead.attribute.log_op == AND) {
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
		}
		break;
	default:
		break;
	}
}
/*<logical OR expression>-> <logical AND expression><logical OR expression’>
FIRST(<logical OR expression>) = {FIRST(<logical AND expression>)}= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_OR_expression(void) {
	logical_AND_expression();
	logical_OR_expression_prime();
}
/*<logical AND expression>-><relational expression><logical AND expression’>
FIRST(<logical AND expression>) = {FIRST(<relational expression>)}= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_AND_expression(void) {
	relational_expression();
	logical_AND_expression_prime();
}
/*<conditional expression>-><logical OR expression>
FIRST(<conditional expression>) = {FIRST(<logical OR expression>)}= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void) {
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}
//relational expressions
/*<primary s_relational expression>-><primary string expression>
FIRST(<primary s_relational expression>) = {FIRST(<primary string expression>)}= {SVID_T,STR_T}
*/
void primary_s_relational_expression(void) {
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");

}
/*<primary a_relational expression>-> AVID_T | FPL_T | INL_T
FIRST(<primary a_relational expression>) = {AVID_T,FPL_T,INL_T}*/
void primary_a_relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	default:
		syn_printe();
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	}
}
/*<relational operators>-> == | <> | > | <
FIRST(<relational operators>) = {==,<>,>,<}*/
void relational_operators(void) {
	switch (lookahead.attribute.rel_op) {
	case EQ:
		match(REL_OP_T, EQ);
		break;
	case NE:
		match(REL_OP_T, NE);
		break;
	case GT:
		match(REL_OP_T, GT);
		break;
	case LT:
		match(REL_OP_T, LT);
		break;
	default:
		syn_printe();
		break;
	}
}
/*<relational expression> -><primary a_relational expression><relational operators><primary a_relational expression> | <primary s_relational expression><relational operators><primary s_relational expression>
FIRST(<relational expression>) = {FIRST(<primary a_relational expression>), FIRST(<primary s_relational expression>)} = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}*/
void relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		relational_operators();
		primary_a_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		relational_operators();
		primary_s_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
		break;
	default:
		syn_printe();
		gen_incode("PLATY: Relational expression parsed");
		break;
	}
}