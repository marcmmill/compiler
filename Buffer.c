/*
* File Name: buffer.c
* Compiler: MS Visual Studio 2017
* Author: Marcus Miller
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 1
* Date: 3 Feburary 2019
* Professor: Sv. Ranev
* Purpose: The source file containing the function definitions from the header file
* Function List: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(),
* b_capacity(), b_mark(), b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(),
* b_eob(), b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(),
* b_rewind(), b_location()
*/
#include "buffer.h"
/*
* Purpose: Allocate memory for the buffer descriptor and initialize it's members
* Author: Marcus Miller
* Version: 1
* Called functions: calloc(), malloc()
* Parameters: 
* short init_capacity, The initial capacity of the buffer, 1 - SHRT_MAX inclusive
* char inc_factor, The increment factor of the buffer, 0 - 255 inclusive
* char o_mode, The operational mode of the buffer, can be a(additive), f(fixed), or m(multiplicative) 
* Return value: Returns the pointer to the buffer on success, will return NULL if not successfull.
* Algorithm: Tests parameters to make sure no invalid cases, will return NULL if any 
* invalid parameters, allocates memory to the buffer and the character buffer, 
* sets the buffer operational mode, the capacity, and the inc_factor of the buffer.
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {
	pBuffer buffer = calloc(1, sizeof(Buffer)); /*Creates a buffer and allocates memory for it*/
	short pos_inc_factor = (unsigned char)inc_factor; /*A non-neative inc_factor converted to a short since unsigned chars cannot be negative*/
	if (init_capacity == 0) { /*If the init_capacity parameter is 0, default will be set depending on the operational mode*/
		buffer->cb_head = malloc(sizeof(char)*DEFAULT_INIT_CAPACITY); /*Allocates memory for the character buffer with teh default capacity*/
		init_capacity = DEFAULT_INIT_CAPACITY;
		if (o_mode == 'a' || o_mode == 'm') { /*sets defaults for both aditive and multiplicative mode*/
			buffer->inc_factor = DEFAULT_INC_FACTOR; /*sets the inc_factor to its default*/
			if (o_mode == 'a') {
				buffer->mode = 1; 
			}
			else
				buffer->mode = -1; 
		}
		else if (o_mode == 'f') { /*sets defaults for fixed mode*/
			buffer->inc_factor = 0; 
			buffer->mode = 0; 
		}
	} else if (init_capacity > 0 && init_capacity <= MAXIMUM_POSITIVE_VALUE) { /*if the init_capacity is not 0 and is valid*/
		buffer->cb_head = malloc(sizeof(char)*init_capacity); /*Allocates memory for the character buffer with the init_capacity*/
		if (pos_inc_factor == 0) {/*sets dafaults for an inc_factor of 0*/
			buffer->mode = FIXED;
			buffer->inc_factor = 0;
		}
		if (o_mode == 'f') { /*sets defaults for fixed mode*/
			buffer->mode = FIXED;
			buffer->inc_factor = 0;
		}
		else if (o_mode == 'a' && pos_inc_factor > 0 && pos_inc_factor < 256) { /*sets defaults for additive mode if valid*/
			buffer->mode = ADDITIVE;
			buffer->inc_factor = inc_factor;
		}
		else if (o_mode == 'm' && pos_inc_factor > 0 && pos_inc_factor < 101) { /*sets defaults for multiplicative mode if valid*/
			buffer->mode = MULTIPLICATIVE;
			buffer->inc_factor = inc_factor;
		}
		else
			return NULL;
	}
	else
		return NULL;
	buffer->capacity = init_capacity; /*sets the buffers capacity to the init_capacity*/
	buffer->flags = DEFAULT_FLAGS; /*Sets the buffers flags to its defaults*/

	if (o_mode == 'f' || o_mode == 'a' || o_mode == 'm') {
		return buffer; /*returns the buffer*/
	}
	else
		return NULL;
}
/*
* Purpose: Add a character to the character buffer
* Author: Marcus Miller
* Version: 1
* Called functions: realloc()
* Parameters:
* pBuffer const pBD, A pointer to the buffer allocated in the first function, must be valid
* char symbol, the symvol to be added to the character buffer, can be any symbol
* Return value: Returns the pointer to the buffer on success, will return NULL if not successfull.
* Algorithm: Tests parameters to make sure they are valid, returns NULL if not valid,
* checks to see if there is room for a character, if there is, adds the character to 
* the buffer and return the pointer to the buffer. If the buffer is full and in 
* fixed mode, returns NULL. If in additive or multiplicative mode,
* calculates a new capacity and attempts to reallocate the character buffer. If 
* successful, adds the character, else it returns NULL.
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	if (pBD == NULL || pBD->cb_head == NULL) {/*Checks for valid buffer*/
		return NULL;
	}
	pBD->flags &= RESET_R_FLAG; /*resets the r flag for future allocation*/
	if (pBD->addc_offset < pBD->capacity) { /*If the buffer is not full*/
		pBD->cb_head[pBD->addc_offset++] = symbol; /*adds the symbol to the addc_offset index of the character buffer*/
		return pBD; /*returns a pointer to the buffer*/
	}
	if(pBD->mode == FIXED) {
		return NULL;
	}
	short newcap = 0; /*newcap used to calculate the new capacity to be given to the buffer*/
	if(pBD->mode == ADDITIVE) {/*If the buffer is full and its in additive mode*/
		if ((newcap = pBD->capacity + (unsigned char)pBD->inc_factor) > 0 && (newcap = pBD->capacity + (unsigned char)pBD->inc_factor) < MAXIMUM_POSITIVE_VALUE) { /*Checks to see if incrementing the buffer by inc_factor will be valid*/
			newcap = pBD->capacity + (unsigned char)pBD->inc_factor; 
		}
		else if ((newcap = pBD->capacity + (unsigned char)pBD->inc_factor) > 0 && (newcap = pBD->capacity + (unsigned char)pBD->inc_factor) > MAXIMUM_POSITIVE_VALUE) { /*If valid but > than the max positive value, max positive value assigned*/
			newcap = MAXIMUM_POSITIVE_VALUE;
		}
		else
			return NULL;
	}
	else if (pBD->mode == MULTIPLICATIVE) {/*If the buffer is full and its in multiplicative mode*/
		if (pBD->capacity == MAXIMUM_POSITIVE_VALUE) {
			return NULL;
		}
		/*Uses the following algorithm to calculate a new capacity for the buffer*/
		short aspace = MAXIMUM_POSITIVE_VALUE - pBD->capacity;
		short newinc = aspace * ((unsigned char)pBD->inc_factor) / 100; /*unsigned char allows values 0 - 255*/
		newcap = pBD->capacity + newinc;
		if (newcap > pBD->capacity && newcap <= MAXIMUM_POSITIVE_VALUE) {
			/*Proceeds with the function*/
		}
		else {
			newcap = MAXIMUM_POSITIVE_VALUE; /*The new capacity is now the highest possible value it can be*/
		}

	}
	else
		return NULL;
	/*The following code tries to reallocate using a temp buffer and assigns it to the real buffer if successfull*/
	char* tmp = (char *)realloc(pBD->cb_head, newcap);
	if (tmp == NULL) {
		return NULL;
	}
	if (tmp != pBD->cb_head) {/*If successfully increased*/
		pBD->flags |= SET_R_FLAG;
		pBD->cb_head = tmp;
	}
	tmp = NULL;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->capacity = newcap;/*capacity is assigned the newly calculated newcap*/
	return pBD;

}
/*
* Purpose: Resets the buffers members so the buffer appears to be empty
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns GOOD(1) on success, or RT_FAIL_1(-1) on failure
* Algorithm: 
*/
int b_clear(Buffer * const pBD) {
	if (pBD == NULL) { /*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	/*Sets the following buffer members to 0 to clear the buffer*/
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags &= RESET_EOB;
	pBD->flags &= RESET_R_FLAG;
	return GOOD;
}
/*
* Purpose: Frees memory from the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: free()
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: N/A
* Algorithm:
*/
void b_free(Buffer * const pBD) {
	free(pBD->cb_head);/*Frees the character buffer*/
	free(pBD);/*Frees the buffer pointer*/

}
/*
* Purpose: Checks if the character buffer is full
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns GOOD(1) if full, 0 if not full, or RT_FAIL_1(-1) on failure
* Algorithm: When capacity = addc_offset, it means that you have reached the end of
* where the characters can be added to the character buffer according to its capacity.
*/
int b_isfull(Buffer * const pBD) {
	if (pBD == NULL) { /*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	if (pBD->capacity == pBD->addc_offset) {/*if the buffer is full, which is when addc_offset = capacity*/
		return GOOD;
	}
	else
		return 0;
}
/*
* Purpose: Gets the current used size of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns addc_offset which is the number of chars in the buffer, 
* or RT_FAIL_1(-1) on failure
* Algorithm:
*/
short b_limit(Buffer * const pBD) {
	if (pBD == NULL) { /*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	return pBD->addc_offset;
}
/*
* Purpose: Gets the capacity of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns the capacity of the buffer, or RT_FAIL_1(-1) on failure
* Algorithm:
*/
short b_capacity(Buffer * const pBD) {
	if (pBD == NULL) { /*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	return pBD->capacity;
}
/*
* Purpose: Sets markc_offset to mark if valid
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: 
* Buffer * const pBD, a constant pointer to a valid buffer
* short mark, the value to be set as markc_offset, must be between 0 - addc_offset inclusive
* Return value: Returns the markc_offset of the buffer, or RT_FAIL_1(-1) on failure
* Algorithm:
*/
short b_mark(pBuffer const pBD, short mark) {
	if (pBD == NULL || pBD->cb_head == NULL) { /*Checks for valid buffer and character buffer*/
		return RT_FAIL_1;
	}
	if (mark >= 0 && mark <= pBD->addc_offset) {/*If the mark is valid, set markc_offset to mark*/
		pBD->markc_offset = mark;
	}
	return pBD->markc_offset;

}
/*
* Purpose: Gets the operational mode of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns the mode of the buffer, or RT_FAIL_2(-2) on failure
* Algorithm:
*/
int b_mode(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_2;
	}
	return pBD->mode;
}
/*
* Purpose: Gets the non-negative inc_factor of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns the non-negative inc_factor of the buffer, or 0x100 on failure
* Algorithm:
*/
size_t b_incfactor(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return 0x100;
	}
	return (unsigned char)pBD->inc_factor;/*returns a non-negative inc_factor of the buffer*/
}
/*
* Purpose: Loads the file into the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: fgetc(), feof(), ungetc(), b_addc()
* Parameters: 
* FILE * const fi, a constant pointer to the input file
* Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns count which is the number of characters loaded into the buffer,
* or RT_FAIL_1(-1) on failure
* Algorithm: Reads a charcter from the file, checks to see if the program is at the end
* of the file, if not, add the symbol to the buffer. Continues to read characters from
* the file until the end of file which if met, ends the loop and returns the number
* of characters added.
*/
int b_load(FILE * const fi, Buffer * const pBD) {
	if (pBD == NULL || fi == NULL) {/*Checks for valid buffer and input file*/
		return RT_FAIL_1;
	}
	int count = 0; /*Count is used to count the amount of characters added*/
	int c = fgetc(fi);/*c is used to hold the gotten characters from the file*/
	while (!feof(fi)) {/*This loop will continuously get and add characters from the file until it reaches the end of the file*/
		if (b_addc(pBD, (char)c)) {
			count++;
			c = fgetc(fi);
		}
		else { /*If character fails to load, ungets character ad returns a load_fail*/
			ungetc(c, fi);
			printf("Last character read from input file is: %c %d\n", c, c);
			return LOAD_FAIL;
		}
	}
	return count;
}
/*
* Purpose: Checks to see if the buffer is empty
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns GOOD(1) if the buffer is empty, 0 if it isn't, or
* RT_FAIL_1(-1) on failure
* Algorithm: addc_offset will be 0 if the buffer is empty since this means the first
* character will be added to the buffer at the 0th position meaning no other character
* has yet been added
*/
int b_isempty(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == 0) {/*if addc_offset is 0, buffer is empty since no characters have been added yet*/
		return GOOD;
	}
	else
		return 0;
}
/*
* Purpose: Gets the character from the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns 0 if getc_offset = addc_offset, the character if they are 
* not equal, or RT_FAIL_2(-2) on failure
* Algorithm: If getc_offset = addc_offset, that means it is the end of the buffer
* so the EOB must be set, else returns the character and increments getc_offset.
*/
char b_getc(Buffer * const pBD) {
	if (pBD == NULL || pBD->cb_head == NULL) {/*Checks for valid buffer and character buffer*/
		return RT_FAIL_2;
	}
	if (pBD->getc_offset == pBD->addc_offset) {/*if at the end of the buffer, set the EOB flag and return 0*/
		pBD->flags |= SET_EOB;
		return 0;
	}
	else {
		pBD->flags &= RESET_EOB;
	}
	return pBD->cb_head[pBD->getc_offset++];/*returns the character at getc_offset then increments getc_offset*/
}
/*
* Purpose: Checks the eob flag of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns either 1 if the flag is set, 0 if not set, 
* or RT_FAIL_1(-1) on failure
* Algorithm:
*/
int b_eob(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	return pBD->flags & CHECK_EOB;/*returns 1 or 0 depending on the eob bit*/

}
/*
* Purpose: Prints the contents of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: b_getc(), b_eob() 
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns count which is the number of characters printed, 0 if the buffer
* is empty, or RT_FAIL_1(-1) on failure
* Algorithm: Checks for valid parameters and if the buffer is empty, gets the first
* character from the buffer and prints it, increments the count, then gets the next
* character until it reaches the end of the buffer where it then sets getc_offset to 0
* and returns the count of characters.
*/
int b_print(Buffer * const pBD) {
	int count = 0;
	if (pBD == NULL || pBD->cb_head == NULL) {/*Checks for valid buffer and character buffer*/
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == 0) {/*buffer is empty*/
		return 0;
	}
	else { /*The following code calls b_getc() and prints the contents of the buffer until it reaches the end of the buffer*/
		char c = b_getc(pBD);/*char c is used to get the character from the buffer*/
		while (!b_eob(pBD)) { //Until reaches end of buffer or runs into run time
			printf("%c", c);
			count++;
			c = b_getc(pBD);
		}
		pBD->getc_offset = 0;/*resets the getc_offset*/
		printf("\n");
	}
	return count;
}
/*
* Purpose: Shortens or expands the buffer to its current size + 1
* Author: Marcus Miller
* Version: 1
* Called functions: realloc()
* Parameters: 
* Buffer * const pBD, a constant pointer to a valid buffer
* char symbol, the symvol to be added to the character buffer, can be any symbol
* Return value: Returns a pointer to the buffer, or NULL on failure
* Algorithm: Checks for valid parameters, resets the r flag to unset(0), creates
* a new capacity of it's current used capacity + 1, it tries to reallocate memory of
* a temp buffer to expand or shrink the buffer. If the new buffer was successfully reallocated, 
* the current buffer is set to the temp buffer, and the new capacity is set to the 
* buffers capacity. Also adds the symbol to the character buffer before returning.
*/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	if (pBD == NULL || pBD->cb_head == NULL) {/*Checks for valid buffer and character buffer*/
		return NULL;
	}
	pBD->flags &= RESET_R_FLAG;/*resets the r flag*/
	/*The following code tries to expand or shrink the capacity to the currently used capacity + 1*/
	short new_cap = (pBD->addc_offset + 1) * sizeof(char);/* the new capacity determined by used capacity + 1*/
	if (new_cap <= 0) { 
		return NULL;
	}
	char* tmpBD = realloc(pBD->cb_head, new_cap);/*Tries to reallocate using the new capacity*/
	if (tmpBD == NULL) {
		return NULL;
	}
	if (tmpBD != pBD->cb_head) {/*If the place in memory changed, set the r flag and set the character buffer to the temp buffer that was reallocated*/
		pBD->cb_head = tmpBD;
		pBD->flags |= SET_R_FLAG;
	}
	tmpBD = NULL;
	pBD->capacity = new_cap;/*new_cap assigned to capacity*/
	pBD->cb_head[pBD->addc_offset++] = symbol;/*symbol added to the end of the character buffer, then addc_offset incremented*/
	return pBD;
}
/*
* Purpose: Checks the r flag of the buffer
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns either 1 if the flag is set, 0 if not set,
* or RT_FAIL_1(-1) on failure
* Algorithm:
*/
char b_rflag(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	return pBD->flags & CHECK_R_FLAG;/*returns 1 or 0 depending on the r flag bit*/
}
/*
* Purpose: Decrements getc_offset
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: returns a decremented getc_offset from the buffer, 
* or RT_FAIL_1(-1) on failure
* Algorithm:
*/
short b_retract(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	return --(pBD->getc_offset);/*decrements, then returns getc_offset*/
}
/*
* Purpose: Sets the getc_offset to the markc_offset
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns the buffers getc_offset, or RT_FAIL_1(-1) on failure
* Algorithm:
*/
short b_reset(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	pBD->getc_offset = pBD->markc_offset;/*Sets getc_offset to markc_offset*/
	return pBD->getc_offset;
}
/*
* Purpose: Gets the buffer's getc_offset
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns the buffers getc_offset, or RT_FAIL_1(-1) on failure
* Algorithm:
*/
short b_getcoffset(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	return pBD->getc_offset;
}
/*
* Purpose: Sets getc_offset and markc_offset to 0 so the buffer can start from the beggining
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns 0 on success, or RT_FAIL_1(-1) on failure
* Algorithm:
*/
int b_rewind(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return RT_FAIL_1;
	}
	/*Resets getc_offset and markc_offset so the buffer can be reread*/
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}
/*
* Purpose: Gets the location of a character in the character buffer at markc_offset
* Author: Marcus Miller
* Version: 1
* Called functions: N/A
* Parameters: Buffer * const pBD, a constant pointer to a valid buffer
* Return value: Returns the character in teh character buffer at the index of
* markc_offset from the buffer, or NULL on failure
* Algorithm:
*/
char * b_location(Buffer * const pBD) {
	if (pBD == NULL) {/*Checks for valid buffer*/
		return NULL;
	}
	return &(pBD->cb_head[pBD->markc_offset]);/*returns the location of the character at index markc_offset*/
}