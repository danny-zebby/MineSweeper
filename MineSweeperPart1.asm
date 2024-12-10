# complete the random number generator (LFSR PRNG) as detailed in course materials. ~
# for this assignment only: ask the user for the Seed value (not zero) for the LFSR x
# ask the user for the game level: beginner, intermediate, advanced or custom x
# use the "card sorting algorithm" discussed in class to populate the screen with mines for the minesweeper game. ~
# each time the user types any character after the field is populated, clear the screen and repopulate it continuing with the current seed. x
#----------------------------------------------------------------------------
# 1. For each grid cell on the game board count the number of mines in the 
# surrounding eight cells. If a cell has no neighboring mines, leave it blank. x
# 2. Each keypress after running should generate another board. x
#----------------------------------------------------------------------------
	.data
s:	.asciiz	"\fEnter the seed for random number generator(can not be zero): "
t:	.asciiz	"\t\t\t\t**MineSweeper**\n"
tt:	.asciiz	"LLLLLLLLL_______\t\t**MineSweeper**\n"
ttt:	.asciiz	"FFFFFFFFF[[[[[[[["
p:	.asciiz	"\nPick a difficulty: Beginner(b), Intermediate(i), Advance(a), Custom(c)"
a1:	.asciiz	"\nPick the number of rows you want(9-24): "	
a2:	.asciiz	"Pick the number of columns you want(9-79): "
a3:	.asciiz	"Pick the number of bombs you want(10-500): "
table:	.word	Ca,Cb,Cc,no,no,no,no,no,Ci
spaces:	.asciiz	" "
bombstorage:	.word 	0:1896
	.extern lfsr,word	
	.code	
taps	= 1<<1|1<<2|1<<28|1
	.globl	main

main:
	la	$a0,s
	syscall	$print_string	# loads our seed prompt
	syscall	$read_char
	beq	$v0,$t1,start 
	syscall	$random
	beqz	$v0,main	# check for zero
	sw	$v0,lfsr($gp)	# places the prompt into the lfsr
start:	la	$a0,p 
	syscall	$print_string	# loads prompt for map
	syscall	$read_char
	ori	$v0,$v0,0x20	# shifts into the upper cases into the lower case ascii
	sub	$t0,$v0,0-'a
	sltiu	$t1,$t0,'j-'a	# checks for both lower and upper
	beqz	$t1,start	# jumps when out of bounds 		
	sll	$t0,$t0,2	# making the letter into word size
	la	$t1,table
	add	$t1,$t1,$t0	# adding letter and address for cases
	lw	$t2,($t1)	
	jr	$t2          	# getting the address to jump to
 	
# s0=mine,s1=row,s2=col
Ca:	li	$s0,99
	li	$s1,16
	li	$s2,30
	b	make
Cb:	li	$s0,10
	li	$s1,9
	li	$s2,9
	b	make
Cc:	la	$a0,a1
	syscall	$print_string
	syscall $read_int
	li	$t0,9		# bound	9-24
	blt	$v0,$t0,Cc
	li	$t0,24
	bgt	$v0,$t0,Cc
	mov	$s1,$v0
Cc2:	la	$a0,a2
	syscall	$print_string
	syscall $read_int
	li	$t0,9		# bound	9-79
	blt	$v0,$t0,Cc2
	li	$t0,79
	bgt	$v0,$t0,Cc2
	mov	$s1,$v0
	mov	$s2,$v0
Cc3:	la	$a0,a3
	syscall	$print_string
	syscall $read_int
	li	$t0,10		# bound	10-500
	blt	$v0,$t0,Cc3
	li	$t0,500
	bgt	$v0,$t0,Cc3
	mov	$s1,$v0
	mov	$s0,$v0
	mul	$t0,$s1,$s2
	bgt	$s0,$t0,start
	b	make
Ci:	li	$s0,40
	li	$s1,16
	li	$s2,16
	b	make
no:	b	start


make:	mov	$t1,$s1
	mov	$t2,$s2
	addi	$a0,$0,'\f
	syscall	$print_char
	la	$a0,t
	syscall	$print_string

# 	li	$a0,0		# make boarder
# 	li	$a1,10   
# 	syscall	$xy
# 	la	$a0,ttt
# 	syscall	$print_string
# 	li	$a0,9
# 	li	$a1,1
# w:	syscall	$xy
# 	mov	$t0,$a0
# 	addi	$a0,$0,'L
# 	syscall	$print_char
# 	mov	$a0,$t0
# 	addi	$a1,$a1,1
# 	bge	$s1,$a1,w

	mov	$t4,$s0		# bomb counter

# Function: PLACES THE BOMBS x
# Inputs: s0 AS BOMB, S1 AS ROW AND S2 AS COL
# OutPuts: NONE ()
BombMaker:	
	jal	rand
	bgezal	$v0,1f
	neg	$v0,$v0	
1:	rem	$a0,$v0,$s2
	jal	rand
	bgezal	$v0,2f	
	neg	$v0,$v0
2:	rem	$a1,$v0,$s1
	addi	$a1,$a1,1
	jal	storeAndCheck	# passes a0 and a1 as arguements
	beqz	$v0,BombMaker	# returns v0 to check if point already used
	syscall	$xy
	addi	$a0,$0,0xf
	syscall	$print_char
	addi	$t4,$t4,-1
	bnez	$t4,BombMaker
# 	syscall	$read_char
# 	jal	clear
# 	b	make

# starting the bomb counter	
	li	$a0,0	# col
	li	$a1,1	# row
loop:	
	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	la	$t1,bombstorage
	add	$s3,$t0,$t1
	lw	$a2,($s3)
	addi	$a3,$0,0xf
	beq	$a2,$a3,j
	jal	BombCounter
BCD:	
	beqz	$v1,j
	syscall	$xy
	mov	$t0,$a0
	mov	$a0,$v1
	syscall	$print_int
	mov	$a0,$t0
j:	addi	$a0,$a0,1
	bne	$a0,$s2,loop
	li	$a0,0
	addi	$a1,$a1,1
	ble	$a1,$s1,loop
# 	syscall	$exit
	syscall	$read_char
	jal	clear
	b	make


# Function: makes random number x
# Inputs: first number to not equal zero
# OutPuts: $v0 with 
rand:	
	lw	$t0,lfsr($gp)	# /* taps 31 30 28 0 */
	li	$v0,taps
	srl	$t1,$t0,1	# (lfsr >> 1)
	andi	$t2,$t0,1	# lfsr & 1
	neg	$t2,$t2		# -(lfsr & 1)
	and	$t2,$t2,$v0	# -(lfsr & 1) & 0xd0000001u
	xor	$v0,$t1,$t2	# (lfsr >> 1) ^ (-(lfsr & 1) & 0xd0000001
	sw	$v0,lfsr($gp) 
	jr	$ra		# return

# Function: STORES BOMB AT POINT POINT AND CHECKS IF NEW POINT x
# Inputs: $A0 AS COL AND $A1 AS ROW
# OutPuts: $V0, 0 OUT OF BOUNDS 1 IN BOUNDS
storeAndCheck:	# a1 holds row(y) a0 holds col(x)
	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	la	$t1,bombstorage
	add	$t1,$t0,$t1	# t1 holds address

# 	sll	$t2,$a0,16
#    	add	$t2,$a1,$t2
	addi	$t2,$0,0xf
	lw	$v0,($t1)
	bne	$t2,$v0,cjump
	mov	$v0,$0
	jr	$ra		# return
cjump:	
	sw	$t2,($t1)
	li	$v0,1
	jr	$ra		# return

# Function: SETS UP THE BOUNDCHECK AND THE NEIGHBOR CHECK
# Inputs: $A0 AS COL AND $A1 AS ROW
# OutPuts: NONE()
BombCounter:
# 	mov	$s4,$ra
	li	$v1,0
	addi	$a2,$a0,1	# col
	addi	$a3,$a1,0	# row
	jal	Boundcheck	# east
	bnez	$v0,BC1
	b	BC11
BC1:	jal	Load8		# load the space next to current
BC11:	addi	$a2,$a0,1	# col
	addi	$a3,$a1,1	# row
	jal	Boundcheck	# south-east
	bnez	$v0,BC2
	b	BC22
BC2:	jal	Load8		# load the space next to current
BC22:	addi	$a2,$a0,0	# col
	addi	$a3,$a1,1	# row
	jal	Boundcheck	# south
	bnez	$v0,BC3
	b	BC33
BC3:	jal	Load8		# load the space next to current
BC33:	addi	$a2,$a0,-1	# col
	addi	$a3,$a1,1	# row
	jal	Boundcheck	# south-west
	bnez	$v0,BC4
	b	BC44
BC4:	jal	Load8		# load the space next to current
BC44:	addi	$a2,$a0,-1	# col
	addi	$a3,$a1,0	# row
	jal	Boundcheck	# west
	bnez	$v0,BC5
	b	BC55
BC5:	jal	Load8		# load the space next to current
BC55:	addi	$a2,$a0,-1	# col
	addi	$a3,$a1,-1	# row
	jal	Boundcheck	# north-west
	bnez	$v0,BC6
	b	BC66
BC6:	jal	Load8		# load the space next to current
BC66:	addi	$a2,$a0,0	# col
	addi	$a3,$a1,-1	# row
	jal	Boundcheck	# north
	bnez	$v0,BC7
	b	BC77
BC7:	jal	Load8		# load the space next to current
BC77:	addi	$a2,$a0,1	# col
	addi	$a3,$a1,-1	# row
	jal	Boundcheck	# north-east
	bnez	$v0,BC8
	b	BC88
BC8:	jal	Load8		# load the space next to current
BC88:	b	BCD

# Function: CHECKS THE BOUND OF SEARCH
# Inputs: $A2 AS COL AND $A3 AS ROW
# OutPuts: $V0, 0 OUT OF BOUNDS 1 IN BOUNDS	
Boundcheck:
# 	addi	$t1,$s2,1
	sltu	$t0,$a2,$s2	# check the bounds of col(x)
	bnez	$t0,Bc1
	mov	$v0,$0		# v0=0 is outof bounds
	jr	$ra		# return
Bc1:	#addi	$t0,$s1,-1
	addi	$t1,$a3,-1
	sltu	$t0,$t1,$s1 	# check the bounds of row(y)
	bnez	$t0,Bc2
	mov	$v0,$0		# v0=0 is outof bounds
	jr	$ra		# return
Bc2:	li	$v0,1		# v0=1 is outof bounds
	jr	$ra		# return

# Function: CHECKS THE NEIGHBORING BOX
# Inputs: $A2 AS COL AND $A3 AS ROW
# OutPuts: $V1 COUNT OF BOMBS
Load8:	addi	$t0,$s2,1
	mul	$t0,$t0,$a3
	add	$t0,$t0,$a2
	sll	$t0,$t0,2
	la	$t1,bombstorage
	add	$t1,$t0,$t1	
	lw	$t0,($t1)
	addi	$t2,$0,0xf
	beq	$t2,$t0,L8
	jr	$ra		# return
L8:	addi	$v1,$v1,1
	jr	$ra		# return
	
# Function: CLEARS THE bombstorage DATA x
# Inputs: NONE
# OutPuts: NONE()
clear:
	mul	$t0,$s1,$s2
	sll	$t1,$s2,2
	add	$t0,$t0,$t1
	mov	$v0,$0
	la	$t1,bombstorage
cloop:	sw	$v0,($t1)
	addi	$t1,$t1,word
	addi	$t0,$t0,-1
	bnez	$t0,cloop
	jr	$ra		# return






# poll:	la	$a0,mouse.flags			# returns mouse click position in v0 and the type of click in v1
# 	addi	$a1,$0,1
# 	syscall	$IO_read
# 	andi	$t0,$v0,mouseFlag.down
# 	beqz	$t0,3f
# 	addi	$a0,$a0,mouse.down-mouse.flags
# 	addi	$a1,$0,4
# 	syscall	$IO_read
# 	sw	$v0,mouseDownXY($gp)
# 	addi	$a0,$a0,word
# 	addi	$a1,$0,1
# 	syscall	$IO_read
# 	sb	$v0,mouseDownButtons($gp)
# 	b	poll
# 3:	andi	$t0,$v0,mouseFlag.up
# 	beqz	$t0,timerCheck
# 	addi	$a0,$a0,mouse.up-mouse.flags
# 	addi	$a1,$0,4
# 	syscall	$IO_read
# 	lw	$t0,mouseDownXY($gp)
# 	bne	$t0,$v0,timerCheck
# 	addi	$a0,$a0,word
# 	addi	$a1,$0,1
# 	syscall	$IO_read
# 	andi	$v0,$v0,mouseButtons.left|mouseButtons.right|mouseButtons.middle
# 	bnez	$v0,timerCheck
# 	lw	$v0,mouseDownXY($gp)
# 	lb	$v1,mouseDownButtons($gp)
# 	jr	$ra		# return	
# timerCheck:
# 	b	poll



	.data
#----------------------------------------------------------------------------
keyboard:	.struct 0xa0000000	#start from hardware base address
flags:		.byte 0
mask:		.byte 0
		.half 0
keypress: 	.byte 0,0,0
presscon: 	.byte 0
keydown:	.half 0
shiftdown:	.byte 0
downcon:	.byte 0
keyup:		.half 0
upshift:	.byte 0
upcon:		.byte 0
		.data
#----------------------------------------------------------------------------
	.extern	mouseDownXY,word
	.extern	mouseDownButtons,word
#_____________________________________________
mouse:		.struct 0xa0000018	#start from mouse base address
flags:		.byte 0
mask:		.byte 0
		.half 0
		.word 0
move:		.word 0,0
down:		.word 0,0
up:		.word 0,0
wheel:		.word 0,0
wheeldown:	.word 0,0
wheelup:	.word 0,0
		.data
mouseFlag:	.struct
move		= 1
down		= 2
up		= 4
wheel		= 8
wheeldown	= 16
wheelup		= 32
		.data
mouseButtons:	.struct
keyShift	= 1
keyAlt		= 2
keyCtrl		= 4
left		= 8
right		= 16
middle		= 32
doubleclick	= 64
		.data
#----------------------------------------------------------------------------
		.data
timer:		.struct 0xa0000050 	#start from timer base address
flags:		.byte 0
mask:		.byte 0
		.half 0
t1:		.word 0
t2:		.word 0
t3:		.word 0
t4:		.word 0
t5:		.word 0
t6:		.word 0
t7:		.word 0

	.code
#----------------------------------------------------------------------------