TITLE STRING PRIMITIVES & MACROS   (Proj6_starkan.asm)

; Author: Anthony Stark	
; Last Modified: 3/13/2024
; OSU email address: starkan@oregonstate.edu
; Course number/section:   CS271 Section: 400
; Project Number: 6                 Due Date: 3/17/2024
; Description: This program will accept 10 integer inputs from the user. These inputs must be converted to SDWORD values and validated. Then the
;				values will be displayed. Following that the sum and average of the values will be calculated and displayed.
; ___________________________________________________________________________________________________________________________________________________


INCLUDE Irvine32.inc

mGetString	MACRO	prompt, str_count, buffer, count
	mov     EDX, prompt
   	Call    WriteString

    	mov     EDX, buffer
	mov	ECX, str_count
	call	ReadString
	mov	count, EAX
	mov	buffer, EDX
ENDM

mDisplayString	MACRO string
	mov	EDX, string
	Call	WriteString
ENDM


ARRAYSIZE = 10			    ; number of integers a user can input
MAX = 12			; max string length of user input to fit in a SDWORD (includes sign and null terminator)
BUFFER = 100                ; large buffer size to enable entry of leading 0s


.data
	intro_message	BYTE		"This program will accept 10 integer values and display them, their sum, and their average.",13,10, \
					"They can be negative or positive values, but must fit inside a SDWORD data type.",0
	user_prompt	BYTE		"Enter a signed integer value: ", 0
	error_prompt	BYTE		"Error: Invalid input. Please try again.", 0
    	display_nums    BYTE        "The numbers you have entered are: ",0
    	display_sum     BYTE        "The sum of these integers is: ",0
    	display_avg     BYTE        "The average of these integers is: ",0
	input_buffer	BYTE		BUFFER DUP(?)						; max characters that can be held in SDWORD data type + 1 for null char
	num_array	SDWORD		10 DUP(?)						; SDWORD array for user input values
   	spacing         BYTE        ", ",0
   	output_buffer   BYTE        MAX DUP(?)                                 			 ; array to hold string output of a SDWORD integer
  	rev_buffer      BYTE        MAX DUP(?)
	sum		SDWORD		?
	avg		SDWORD		?
	byte_count	DWORD		0							; counts the number of characters in string
	goodbye		BYTE		"Thank you for testing this program. Goodbye.",0
	

.code
main PROC
	push	OFFSET intro_message
	call	introduction

	mov	ECX, ARRAYSIZE		; sets loop counter to number of user inputs
	xor     EBX, EBX
ReadIntLoop:
	push	ECX			; saves loop counter value
	push	OFFSET error_prompt
	push	byte_count
	push	OFFSET num_array
	push	OFFSET input_buffer
	push	OFFSET user_prompt
	call	ReadVal
	pop	ECX			; restores loop counter
	loop	ReadIntLoop

    	push    OFFSET rev_buffer
   	push    OFFSET output_buffer
   	push    OFFSET spacing
    	push    OFFSET display_nums
    	push    OFFSET num_array
    	call    PrintArray

    	push    OFFSET sum
    	push    OFFSET num_array
	call	SumCalc

    	push    OFFSET rev_buffer
    	push    OFFSET output_buffer
    	push    OFFSET display_sum
    	push    sum
    	call    PrintSum

    	push    OFFSET avg
    	push    sum
	call	AvgCalc

    	push    OFFSET rev_buffer
    	push    OFFSET output_buffer
   	push    OFFSET display_avg
    	push    avg
    	call    PrintAvg

	push	OFFSET goodbye
	call	Farewell
	Invoke ExitProcess,0	; exit to operating system
main ENDP

; _______________________________________________________________________________________________________________
; Name: introduction
;
; Greets the user and explains the purpose of the program.
;
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= address of array (string)
;
; Returns: 
;			None
; _________________________________________________________________________________________________________________
introduction PROC
	push	EBP
	mov	EBP, ESP
	mov	EDX, [EBP + 8]		; accesses intro_message address on stack
	call	WriteString
	call	CrLf
	call	CrLf
	pop	EBP
	RET	4
introduction ENDP

; _______________________________________________________________________________________________________________
; Name: ReadVal
;
; Invokes mGetString macro and converts string of ASCII numbers to SDWORD representation
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;           [EBP + 28]  = counter for num_array index
;			[EBP + 24]	= error_prompt: stores a string to be displayed in case of error
;			[EBP + 20]  = byte_count: stores the number of characters in user input
;			[EBP + 12]  = input_buffer: holds user input
;			[EBP + 8]	= user_prompt: address of array (string)
;
; Returns: 
;			[EBP + 16]  = num_array: stores integer values of user input
; _________________________________________________________________________________________________________________
ReadVal PROC
	push    EBP
	mov     EBP, ESP

	mov	EDI, [EBP + 16]        
    

ValErrorLoop:
    	mGetString  [EBP + 8], BUFFER, [EBP+12], [EBP+20]      	; gets user input

    	mov     ESI, [EBP + 12]        
    	mov     ECX, [EBP + 20]        
	call    Validate                                ; Validate the input string
    	cmp     EAX, 0                          
    	jnz     Error                                   ; If input is invalid display error message and reprompt

    	mov     ESI, [EBP + 12]        
    	mov     ECX, [EBP + 20]        
	call    ConvertString                   ; Convert string to SDWORD and verify value is in range
    	cmp     EAX, 0
    	jnz     Error                           ; if input is out of range display error and reprompt

    	pop     EBP
    	RET     20

Error:                                          ; Display error message
    	mDisplayString [EBP + 24]      
    	call    CrLf
    	jmp     ValErrorLoop                    

ReadVal ENDP

; _______________________________________________________________________________________________________________
; Name: Validate
;
; Checks user input to ensure that it is a string representation of a valid integer that will fit in a SDWORD
;
; Preconditions: 
;               ECX = length of input buffer
;               ESI = input buffer memory address
;               EDI = output array memory address
;
; Postconditions:None
;
; Receives: None
;			
;
; Returns: 
;			EAX = 1 if user input is invalid
;           EAX = 0 if user input is valid
; _________________________________________________________________________________________________________________
Validate PROC
    push    EBP
    mov     EBP, ESP

    cmp     ECX, 0                  ; Check for empty input = invalid
    je      InvalidInput            

    cmp     ECX, MAX
    jg      InvalidInput

    ; Check for leading + or -
    mov     AL, [ESI]               
    cmp     AL, '+'                 
    je      CheckNextCharacter   
    cmp     AL, '-'                 
    je      CheckNextCharacter    

    ; Check if the first character is a digit - invalid if not
    cmp     AL, '0'
    jl      InvalidInput            
    cmp     AL, '9'
    jg      InvalidInput            

CheckNextCharacter:                 ; Move to the next character
    inc     ESI
    dec     ECX

    ; Check remaining characters
    mov     EDX, 0                  
    xor     EAX, EAX                

CheckCharactersLoop:                ; checks that all other characters are between 0 and 9 - otherwise invalid
    cmp     ECX, 0                  
    je      ValidInput              
    mov     AL, [ESI]               
    cmp     AL, '0'                
    jl      InvalidInput            
    cmp     AL, '9'
    jg      InvalidInput            

HandleSign:                         ; ensures sign value doesn't interfere with conversion to SDWORD
    inc     ESI
    dec     ECX
    jmp     CheckCharactersLoop

ValidInput:
    pop     EBP
    xor     EAX, EAX                ; Return 0 to indicate success
    RET

InvalidInput:
    pop     EBP
    mov     EAX, 1                  ; Return 1 to indicate failure
    RET

Validate ENDP

; _______________________________________________________________________________________________________________
; Name: ConvertString
;
; Converts ascii representation of an integer to a SDWORD integer data type
;
; Preconditions: 
;               ECX = length of input buffer
;               ESI = input buffer memory address
;               EDI = output array memory address
;
; Postconditions:None
;
; Receives: None
;			
;
; Returns: 
;			EDI = ouput array memory address containing integer SDWORD values
; _________________________________________________________________________________________________________________
ConvertString PROC
    push    EBP
    mov     EBP, ESP

    ; Check for leading '+' or '-'
    mov     AL, [ESI]               
    cmp     AL, '+'                 
    je      SignDetected            
    cmp     AL, '-'                 
    je      ConvertNegativeNumber           

    jmp     ConvertPositiveNumber

ConvertNegativeNumber:
    inc     ESI
    dec     ECX
    xor     EDX, EDX               
    xor     EAX, EAX                

ConvertNegLoop:                         ; converts negative string to SDWORD data type
    LODSB
    cmp     AL, 0                   
    je      DoneNeg                    ; conversion is done if null terminator found

    sub     AL, '0'            
    imul    EDX, EDX, 10         
    add     EDX, EAX            
    jo      OverflowError              

    loop    ConvertNegLoop  

DoneNeg:                            ; stores negative SDWORD value in output array
    imul    EDX, EDX, -1
    jmp     Done

SignDetected:                   ; handles strings with a leading + sign
    inc     ESI
    dec     ECX

ConvertPositiveNumber:          
    xor     EDX, EDX           
    xor     EAX, EAX               

ConvertLoop:                        ; converts positive string to SDWORD
    LODSB
    cmp     AL, 0                   
    je      Done                    ; finished converting if null terminator found

    sub     AL, '0'                
    imul    EDX, EDX, 10            
    add     EDX, EAX      
    jo      OverflowError           
    loop    ConvertLoop            

Done:                                ; Store the converted SDWORD in the output array
    mov     [EDI + EBX], EDX
    add     EBX, SIZEOF SDWORD
    mov     EAX, 0

    pop     EBP
    RET

OverflowError:                      ; returns flag (1 in EAX) if overflow detected - doesn't fit in SDWORD
    mov     EAX, 1
    pop     EBP
    RET

ConvertString ENDP

; _______________________________________________________________________________________________________________
; Name: WriteVal
;
; Converts an SDWORD integer back to an ASCII string and prints the output
;
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;           [EBP + 12]  = input SDWORD
;
; Returns: 
;			[EBP + 8]  = output buffer containing SDWORD as backward string
;           [EBP + 16] = output buffer containing SDWORD as correct string
; _________________________________________________________________________________________________________________

WriteVal PROC
    push    EBP
    mov     EBP, ESP

    push    [EBP + 8]
    call    ClearBuffer         ; clears buffer before accessing next element in array

    push    [EBP + 16]
    call    ClearBuffer         ; clears buffer before accessing next element in array

    mov     EBX, 10
    xor     ECX, ECX
    mov     EAX, [EBP + 12]
    mov     EDI, [EBP + 8]
    
    cmp     EAX, 0
    jl      negVal
    push    EAX
    jmp     convertLoop

negVal:                         ; if negative stores abs value of int in EDX and adds - sign to EDI
    mov     EDX, EAX
    imul    EDX, EDX, -1
    mov     EAX, '-'
    push    EAX
    mov     EAX, EDX

convertLoop:                ; converts SDWORD to backwards ascii string of value
    CDQ
    div     EBX
    push    EAX
    mov     al, dl
    add     al, '0'
    STOSB
    inc     ECX
    pop     EAX
    cmp     EAX, 0
    jne     convertLoop
    
    pop     EAX
    cmp     EAX, '-'
    je      negChar

printString:                    ; adds null terminator and sets up registers for reverseLoop
    mov     al, 0
    STOSB

    mov     ESI, [EBP+8]        ; ECX holds length of string
    mov     EDI, [EBP + 16]           
    add     ESI, ECX
    
reverseLoop:                    ; Swap characters from the beginning and end
    xor     EAX, EAX
    dec     ESI             
    mov     AL, [ESI]
    STOSB                   
    cmp     ESI, [EBP+8]           ; Compare ESI with the start address of the string
    jne     reverseLoop             ; Repeat until ESI reaches the start of the string

done:
    mDisplayString  [EBP+16]
    pop     EBP             
    ret     12               

negChar:
    mov     al, '-'
    STOSB
    inc     ECX
    jmp     printString

WriteVal ENDP

; _______________________________________________________________________________________________________________
; Name: ClearBuffer
;
; Replaces a buffer with a value of 0 so residual data does not affect future ouput
;
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;           [EBP + 8]  = input buffer
;
; Returns: 
;			[EBP + 8]  = cleared buffer
;           
; _________________________________________________________________________________________________________________
ClearBuffer PROC
    push    EBP
    mov     EBP, ESP

    mov     ECX, MAX
    mov     EDI, [EBP + 8]
    mov     EAX, 0
    REP     STOSB

    pop     EBP
    ret     4

ClearBuffer ENDP

; _______________________________________________________________________________________________________________
; Name: PrintArray
;
; Prints the array of SDWORD integers
;
; Preconditions: num_array contains a SDWORD array of length ARRAYSIZE
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= address of a SDWORD array
;           [EBP + 12]  = display message
;           [EBP + 16]  = delimeter for values in array
;
; Returns: 
;			[EBP + 20]  = output buffer for backward strings
;           [EBP + 24]  = output buffer for corrected strings
; _________________________________________________________________________________________________________________
PrintArray PROC
    push    EBP
    mov     EBP, ESP

    mDisplayString [EBP + 12]
    mov     ECX, ARRAYSIZE
    mov     ESI, [EBP + 8]

arrayLoop:                  ; prints out string of each number in array
    LODSD
    push    ESI
    push    ECX

    push    [EBP + 24]
    push    EAX
    push    [EBP + 20]
    call    WriteVal

    pop     ECX
    pop     ESI
    dec     ECX
    cmp     ECX, 0
    je      Done

    mDisplayString [EBP + 16]   ; prints delimeter
    jmp    arrayLoop

Done:
    call    CrLf
    pop     EBP
    RET     20
PrintArray ENDP


; _______________________________________________________________________________________________________________
; Name: SumCalc
;
; Calculates the sum of the user entered integers
;
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= num_array address (SDWORD array)
;
; Returns: 
;			[EBP + 12]  = sum (SDWORD)
; _________________________________________________________________________________________________________________
SumCalc	PROC
	push	EBP
	mov		EBP, ESP
    	mov     ESI, [EBP + 8]
    	mov     EDI, [EBP + 12]
    	mov     ECX, ARRAYSIZE
    	xor     EDX, EDX
    	CLD

SumLoop:
    	LODSD
    	add     EDX, EAX
    	loop    sumLoop

    	mov     [EDI], EDX


	pop		EBP
	RET     8
SumCalc ENDP

; _______________________________________________________________________________________________________________
; Name: PrintSum
;
; Prints the sum of user input integers
;
; Preconditions: sum has been calculated by SumCalc
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= sum value (SDWORD)
;           [EBP + 12]  = display message
;
; Returns: 
;			[EBP + 16]  = output buffer with backward string
;           [EBP + 20]  = output buffer with correct string
; _________________________________________________________________________________________________________________
PrintSum PROC
    push    EBP
    mov     EBP, ESP
    mDisplayString [EBP + 12]
    
    push    [EBP + 20]
    push    [EBP + 8]
    push    [EBP+16]
    call    WriteVal

    call    CrLf
    pop     EBP
    RET     16
PrintSum ENDP

; _______________________________________________________________________________________________________________
; Name: AvgCalc
;
; Calculates the average of the user entered integers
;
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= sum (SDWORD)
;
; Returns: 
;           [EBP + 12]  = avg (SDWORD)
; _________________________________________________________________________________________________________________
AvgCalc PROC
	push	EBP
	mov		EBP, ESP
	; move sum into EAX and ARRAYSIZE into EBX and divide, check EDX for remainder to figure out rounding
    	xor     EDX, EDX
    	mov     EAX, [EBP + 8]
    	mov     EDI, [EBP + 12]

    	cdq
   	mov     EBX, ARRAYSIZE
    	idiv    EBX
    	shr		EBX, 1					; shifts bits 1 to the right, dividing the divisor by 2
    	cmp     EDX, 0
    	jge     positiveRound
    	jl      negativeRound

positiveRound:
    	cmp		EDX, EBX				; compares remainder and (divisor/2), if remainder is greater, rounds up, if not rounds down
    	jge		RoundUp
    	jl      AvgDone

RoundUp:
    	inc     EAX
    	jmp     AvgDone

negativeRound:
    	neg     EDX
    	cmp		EDX, EBX				; compares remainder and (divisor/2), if remainder is greater, rounds up, if not rounds down
   	jge		RoundDown
    	jl      AvgDone

RoundDown:
    	dec     EAX
    	jmp     AvgDone

AvgDone: 
    	mov     [EDI], EAX
    	pop		EBP
	RET     8

AvgCalc ENDP

; _______________________________________________________________________________________________________________
; Name: PrintAvg
;
; Prints the average of user input integers
;
; Preconditions: avg has been calculated by AvgCalc
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= avg value (SDWORD)
;           [EBP + 12]  = display message
;
; Returns: 
;			[EBP + 16]  = output buffer with backward string
;           [EBP + 20]  = output buffer with correct string
; _________________________________________________________________________________________________________________
PrintAvg PROC
    push    EBP
    mov     EBP, ESP
    mDisplayString [EBP + 12]
    
    push    [EBP + 20]
    push    [EBP + 8]
    push    [EBP + 16]
    call    WriteVal

    call    CrLf
    pop     EBP
    RET     16
PrintAvg ENDP

; _______________________________________________________________________________________________________________
; Name: Farewell
;
; Displays a goodbye message to the user
;
; Preconditions: None
;
; Postconditions:None
;
; Receives:
;			[EBP + 8]	= address of array (string)
;
; Returns: 
;			None
; _________________________________________________________________________________________________________________
Farewell PROC
	push	EBP
	mov	EBP, ESP
	mov	EDX, [EBP + 8]	
	call	WriteString
	pop	EBP
	RET	4
Farewell ENDP

END main
