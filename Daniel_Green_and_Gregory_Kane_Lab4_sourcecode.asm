;***********************************************************
;*	ECE375 Lab 4
;*
;*	 Author: Gregory Kane & Daniel Green
;*	   Date: 2/7/2025
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	rm = r14
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

; Registers 5-12 are for intermediate steps, r13 is for final carry of multiplication
.def	mul24_op1_1 = r5
.def	mul24_op1_2 = r6
.def	mul24_op1_3 = r7
.def	mul24_op2_1 = r8
.def	mul24_op2_2 = r9
.def	mul24_op2_3 = r10
.def    mul24_st_cnt = r20

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine

		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr
		clr		zero			; Set the zero register to zero, maintain
										; these semantics, meaning, don't
										; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program

		; Call function to load ADD16 operands
		rcall ADD16PRE; Check load ADD16 operands (Set Break point here #1)

		; Call ADD16 function to display its results (calculate FCBA + FFFF)
		rcall ADD16RES; Check ADD16 result (Set Break point here #2)


		; Call function to load SUB16 operands
		; At this point, registers 26->31 are primed with the operands
		; Indirection will be used to pull the operands from the addresses in data memory
		rcall SUB16PRE; Check load SUB16 operands (Set Break point here #3)

		; Call SUB16 function to display its results (calculate FCB9 - E420)
		rcall SUB16RES ; Check SUB16 result (Set Break point here #4)

		rcall MUL16

		; Call function to load MUL24 operands
		rcall MUL24PRE; Check load MUL24 operands (Set Break point here #5)

		 ; Call MUL24 function to display its results (calculate FFFFFF * FFFFFF)
		rcall MUL24RES  ; Check MUL24 result (Set Break point here #6)

		; Call the COMPOUND function
		rcall COMPOUNDRES ; Check COMPOUND result (Set Break point here #8)

DONE:	rjmp	DONE			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;-----------------------------------------------------------
ADD16PRE:
		; Load beginning address of first operand into X
		ldi		ZL, low(OperandA<<1)	; Load low byte of address
		ldi		ZH, high(OperandA<<1)	; Load high byte of address
		lpm r0, Z+
		lpm r1, Z

		; Load beginning address of second operand into Y
		ldi		ZL, low(OperandB<<1)
		ldi		ZH, high(OperandB<<1)
		lpm r2, Z+
		lpm r3, Z
		ret

ADD16RES:
		; Execute the function
		add		r2, r0 ; sum low bytes of operands, store to r2
		adc		r3, r1 ; sum high bytes of operands, store to r3
		brcs	ADD16_CARRY		; handle carry
		ret						; End a function with RET

ADD16_CARRY:
		inc r4 ; add 1 to register value if carry occurred 

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;       result. Always subtracts from the bigger values.
;-----------------------------------------------------------
SUB16PRE:
		; Load beginning address of first operand into X
		ldi		ZL, low(OperandC<<1)	; Load low byte of address
		ldi		ZH, high(OperandC<<1)	; Load high byte of address
		lpm r0, Z+
		lpm r1, Z

		; Load beginning address of second operand into Y
		ldi		ZL, low(OperandD<<1)
		ldi		ZH, high(OperandD<<1)
		lpm r2, Z+
		lpm r3, Z
		ret

SUB16RES:
		sub		r0, r2 ; subtract low bytes of operands, store to r0
		sbc		r1, r3 ; subtract high bytes of operands, store to r1
		ret			   ; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit
;       result.
;-----------------------------------------------------------
MUL24PRE:

		; load operand values into memory for use
		; start with first 3 bytes (E1/E2(l))
		ldi ZL, low(OperandE1<<1)
		ldi ZH, high(OperandE1<<1)
		lpm mul24_op1_1, Z+
		lpm mul24_op1_2, Z
		ldi ZL, low(OperandE2<<1)
		ldi ZH, high(OperandE2<<1)
		lpm mul24_op1_3, Z

		; next three bytes (F1/F2(l))
		ldi ZL, low(OperandF1<<1)
		ldi ZH, high(OperandF1<<1)
		lpm mul24_op2_1, Z+
		lpm mul24_op2_2, Z
		ldi ZL, low(OperandF2<<1)
		ldi ZH, high(OperandF2<<1)
		lpm mul24_op2_3, Z
		ret

MUL24RES:
/*		; Preserve register states before starting 24bit multiplication
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop
		clr		zero*/

		; prime Z with destination memory locaiton after clearing:
		ldi ZL, low(MUL24_Result)
		ldi ZH, high(MUL24_Result)
		ldi	mpr, 6
		clr r1
		
clear_dest:
		st Z+, r1         ; zero-out current byte of Z
		dec mpr			  ; decrement loop counter
		brne clear_dest   ; are we finished?
						  ; otherwise, continue on 
		
		; We'll be using the registers r21 -> r26 to contain intermediate values
		; These will eventually be passed to the destination one byte at a time

		; in practice: 
		;  multiply operands, which are placed in R0,R1
		;  add to intermediate value registers these results
		;  add to NEXT low byte, the value of any carry(s) leftover
		; 
		; since some steps can potentially generate TWO carry values, we account for this.

		; First set, R21->R26
		mul mul24_op1_1, mul24_op2_1 ; E1 low * F1 low (no carry handling)
		add r21, r0					 ; place LSB of result in R21
		adc r22, r1					 ; place MSB of result in R22
		
		mul mul24_op1_1, mul24_op2_2 ; E1 low * F1 mid
		add r22, r0
		adc r23, r1						 ; clears the carry for next add
		
		
		mul mul24_op1_1, mul24_op2_3 ; E1 low * F1 high
		add r23, r0
		adc r24, r1

		; Second set, R23->R28
		mul mul24_op1_2, mul24_op2_1 ; E1 mid * F1 low
		add r22, r0
		add r23, r1
		;adc r25, r0

		mul mul24_op1_2, mul24_op2_2 ; E1 mid * F1 mid
		adc r23, r0
		add r24, r1

		mul mul24_op1_2, mul24_op2_3 ; E1 mid * F1 high
		adc r24, r0
		adc r25, r1
		inc r25   ;accounts for carry

		; Third set, R25->R30
		mul mul24_op1_3, mul24_op2_1 ; E1 high * F1 low
		add r23, r0
		add r24, r1
		;adc r27, r0

		mul mul24_op1_3, mul24_op2_2 ; E1 high * F1 mid
		adc r24, r0
		add r25, r1

		mul mul24_op1_3, mul24_op2_3 ; E1 high * F1 high
		adc r25, r0
		adc r26, r1 
		inc r26  ;accounts for carry

		; prime Z reg with destination addres and
		; store all 6 bytes (lo+hi per each) into the result location, post-increment
		ldi ZL, low(MUL24_Result)
		ldi ZH, high(MUL24_Result)
		st Z+, r21
		st Z+, r22
		st Z+, r23
		st Z+, r24
		st Z+, r25
		st Z+, r26
		st Z+, r27
		st Z+, r28
		st Z+, r29
		st Z, r30
		ret

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((G - H) + I)^2
;       by making use of SUB16, ADD16, and MUL24.
;
;       D, E, and F are declared in program memory, and must
;       be moved into data memory for use as input operands.
;
;       All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUNDRES:
		
		; Setup SUB16 with operands G and H
		ldi		ZL, low(OperandG<<1)	; Load low byte of address
		ldi		ZH, high(OperandG<<1)	; Load high byte of address
		lpm r0, Z+
		lpm r1, Z

		; Load beginning address of second operand into Y
		ldi		ZL, low(OperandH<<1)
		ldi		ZH, high(OperandH<<1)
		lpm r2, Z+
		lpm r3, Z

		; Perform subtraction to calculate G - H
		call SUB16RES ; now r1,high/r0,low have results

		; Setup the ADD16 function with SUB16 result and operand I
		; Load beginning address of second operand into Y (first operand already in correct location)
		ldi		ZL, low(OperandI<<1)
		ldi		ZH, high(OperandI<<1)
		lpm r2, Z+
		lpm r3, Z

		; Perform addition next to calculate (G - H) + I
		call ADD16RES

		; Setup the MUL24 function with ADD16 result as both operands
		; Perform multiplication to calculate ((G - H) + I)^2
		; (r4,r3,r2 -> 16 bit result produced by ADD16)
		mov mul24_op1_1, r2
		mov mul24_op1_2, r3
		mov mul24_op1_3, r4
		mov mul24_op2_1, r2
		mov mul24_op2_2, r3
		mov mul24_op2_3, r4

		call MUL24RES

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;       A - Operand A is gathered from address $0101:$0100
;       B - Operand B is gathered from address $0103:$0102
;       Res - Result is stored in address
;             $0107:$0106:$0105:$0104
;       You will need to make sure that Res is cleared before
;       calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;*	Do not  section.
;***********************************************************
; ADD16 operands
OperandA:
	.DW 0xFCBA
OperandB:
	.DW 0xFFFF

; SUB16 operands
OperandC:
	.DW 0XFCB9
OperandD:
	.DW 0XE420

; MUL24 operands
OperandE1:
	.DW	0XFFFF
OperandE2:
	.DW	0X00FF
OperandF1:
	.DW	0XFFFF
OperandF2:
	.DW	0X00FF

; Compoud operands
OperandG:
	.DW	0xFCBA				; test value for operand G
OperandH:
	.DW	0x2022				; test value for operand H
OperandI:
	.DW	0x21BB				; test value for operand I

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.
.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$0130
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0140				; data memory allocation for results
SUB16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$0150
MUL24_OP1:
		.byte 3			
MUL24_OP2:
		.byte 3				

.org	$0160				; data memory allocation for results
MUL24_Result:
		.byte 6				; allocate three bytes for ADD16 result
;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
