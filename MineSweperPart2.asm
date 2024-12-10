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
# 1. hide the mines and counts
# 2. have left clicks that are on the board "clear" the hidden symbol with the 
# 	hidden information
# 3. If the hidden cell is blank, then automatically click on all 8 neighboring 
# 	cells, revealing their contents. (this should be recursive, so that if 
# 	another blank is found it will cause more revealing)
# 4. if a left click reveals a mine, then end the game.
# 5. right clicks should flag the cell without revealing the underlying 
# 	contents, and decrease the displayed mine count. A right click on a cell 
# 	already flagged should remove the flag and increase the count.
# 6. if the displayed mine count goes to zero, check to see if all flags are on 
# 	mines (not misplaced), if so win! otherwise just keep playing.
# 7. also keep track of how many cells have been cleared, either to blank or a 
# 	number, so if that count reaches the total cells minus mines the game 
# 	will also win.
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
bombBoard:	.word 	0:1896
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
Ca:	li	$s0,99		# stores bombs
	li	$s1,16		# stores row
	li	$s2,30		# stores col
	b	make
Cb:	li	$s0,10		# stores bombs
	li	$s1,9		# stores row
	li	$s2,9		# stores col
	b	make
Cc:	la	$a0,a1
	syscall	$print_string
	syscall $read_int
	li	$t0,9		# bound	9-24
	blt	$v0,$t0,Cc
	li	$t0,24
	bgt	$v0,$t0,Cc
	mov	$s1,$v0		# stores row
Cc2:	la	$a0,a2
	syscall	$print_string
	syscall $read_int
	li	$t0,9		# bound	9-79
	blt	$v0,$t0,Cc2
	li	$t0,79
	bgt	$v0,$t0,Cc2
	mov	$s2,$v0		# stores col
Cc3:	la	$a0,a3
	syscall	$print_string
	syscall $read_int
	li	$t0,10		# bound	10-500
	blt	$v0,$t0,Cc3
	li	$t0,500
	bgt	$v0,$t0,Cc3
	mul	$t0,$s1,$s2	# check if bombs < 50% area
	srl	$t0,$t0,1
	bgt	$v0,$t0,Cc3
	mov	$s0,$v0		# stores bombs
	mul	$t0,$s1,$s2
	bgt	$s0,$t0,start
	b	make
Ci:	li	$s0,40		# stores bombs
	li	$s1,16		# stores row
	li	$s2,16		# stores col
	b	make
no:	b	start

# Function: CLEAR SCRREN AND PLACES TILE
# Inputs: s0 AS BOMB, S1 AS ROW AND S2 AS COL
# OutPuts: NONE ()
make:	addi	$a0,$0,'\f
	syscall	$print_char	# clear screen
	la	$a0,t
	syscall	$print_string	# place title

# Function: PLACES THE BOMBS 
# Inputs: NONE
# OutPuts: NONE
	mov	$t4,$s0		# bomb counter
	la	$s4,bombBoard
BombMaker:	
	jal	rand
	bgezal	$v0,1f
	neg	$v0,$v0	
1:	rem	$a0,$v0,$s2	# get random col
	jal	rand
	bgezal	$v0,2f	
	neg	$v0,$v0
2:	rem	$a1,$v0,$s1	# get random row
	addi	$a1,$a1,1
	jal	storeAndCheck	# passes a0 and a1 as arguements
	beqz	$v0,BombMaker	# returns v0 to check if point already used
# 	syscall	$xy		# x,y of bomb
# 	addi	$a0,$0,0xf
# 	syscall	$print_char	# prints bomb
	addi	$t4,$t4,-1	# iterate bomb count
	bnez	$t4,BombMaker

# Function: COUNTS THE BOMBS 
# Inputs: NONE
# OutPuts: NONE	
	li	$a0,0	# col
	li	$a1,1	# row
loop:	
	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	add	$s3,$t0,$s4	# gets address of point
	lw	$a2,($s3)
	addi	$a3,$0,0xf	# loads bomb
	beq	$a2,$a3,j	# if point is a bomb then jump	
	jal	BombCounter	
BCD:	
	beqz	$v1,j		# if a bomb was found cont
	syscall	$xy		# x,y of counter
	mov	$t0,$a0
	mov	$a0,$v1
	syscall	$print_int	# prints counter
	mov	$a0,$t0
j:	addi	$a0,$a0,1	# iterate col
	bne	$a0,$s2,loop
	li	$a0,0		# restock col
	addi	$a1,$a1,1	# iterate row
	ble	$a1,$s1,loop
	syscall	$read_char	# read for loop
	jal	clear		
	b	make		# make new board



# Function: MAKES RANDOM NUMBER 
# Inputs: FIRST NUMBER TO NOT EQUAL ZERO 
# OutPuts: $V0-RANDOM NUMBER
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

# Function: STORES BOMB AT POINT POINT AND CHECKS IF NEW POINT 
# Inputs: $A0 AS COL AND $A1 AS ROW
# OutPuts: $V0, 0 OUT OF BOUNDS 1 IN BOUNDS
storeAndCheck:
	addi	$t0,$s2,1	# store
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2	# gets address of point
	add	$t1,$t0,$s4	# t1 holds address
	addi	$t2,$0,0xf	# check
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
# OutPuts: $V1 COUNT OF BOMBS
BombCounter:
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
	add	$t1,$t0,$s4	# gets address of point
	lw	$t0,($t1)
	addi	$t2,$0,0xf	# checks for bomb
	beq	$t2,$t0,L8
	jr	$ra		# return
L8:	addi	$v1,$v1,1
	jr	$ra		# return
	
# Function: CLEARS THE bombBoard DATA x
# Inputs: NONE
# OutPuts: NONE()
clear:
	mul	$t0,$s1,$s2
	sll	$t1,$s2,2
	add	$t0,$t0,$t1	# finding the points to clear
	mov	$v0,$0
	la	$t1,bombBoard	
cloop:	sw	$v0,($t1)	# clearing out the area
	addi	$t1,$t1,word
	addi	$t0,$t0,-1	# interate
	bnez	$t0,cloop	# loops
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