# 1. complete the random number generator (LFSR PRNG) as detailed in course 
# materials. x
# 2. for this assignment only: ask the user for the Seed value (not zero) for 
# the LFSR x
# 3. ask the user for the game level: beginner, intermediate, advanced or custom x
# 4. use the "card sorting algorithm" discussed in class to populate the screen 
# with mines for the minesweeper game. x
# 5. each time the user types any character after the field is populated, clear 
# the screen and repopulate it continuing with the current seed. x
#----------------------------------------------------------------------------
# 1. For each grid cell on the game board count the number of mines in the 
# surrounding eight cells. If a cell has no neighboring mines, leave it blank. x
# 2. Each keypress after running should generate another board. x
#----------------------------------------------------------------------------
# 1. hide the mines and counts x
# 2. have left clicks that are on the board "clear" the hidden symbol with the 
# hidden information x
# 3. If the hidden cell is blank, then automatically click on all 8 neighboring 
# cells, revealing their contents. (this should be recursive, so that if 
# another blank is found it will cause more revealing)
# 4. if a left click reveals a mine, then end the game. x
# 5. right clicks should flag the cell without revealing the underlying 
# contents, and decrease the displayed mine count. A right click on a cell 
# already flagged should remove the flag and increase the count. x
# 6. if the displayed mine count goes to zero, check to see if all flags are on 
# mines (not misplaced), if so win! otherwise just keep playing. x
# 7. also keep track of how many cells have been cleared, either to blank or a 
# number, so if that count reaches the total cells minus mines the game 
# will also win. x
#----------------------------------------------------------------------------
# 1. Add a counter to count down the unmined cells that the player has cleared. 
# If it reachs zero, game is won. x
# 2. check the win status if the number of flags remaining is zero. ( one or more 
# could be in the wrong place!) x
# 3. add the timer operation, displaying the time since the first click every 
# second. x
# 4. Add the split-click or middle click behavior for clearing neighbors around 
# "satisfied" numbers. x
# 5. Add a Win (or Loss) dialog. x
#----------------------------------------------------------------------------
# 1. Add file based record keeping for best time for each level Beginner, 
# Intermediate, Advanced.( you may omit custom records)
#----------------------------------------------------------------------------
	.data
s:	.asciiz	"\fEnter the seed for random number generator(can not be zero): "
p:	.asciiz	"Pick a difficulty: Beginner(b), Intermediate(i), Advance(a), Custom(c)"
a1:	.asciiz	"\nPick the number of rows you want(9-24): "	
a2:	.asciiz	"Pick the number of columns you want(9-79): "
a3:	.asciiz	"Pick the number of bombs you want(10-500): "
t:	.asciiz	"Timer:\t     Bombs:\tUncleared:\t**MineSweeper**\n"
w:	.asciiz	"You Win (Press any)"
l:	.asciiz	"You Lose (Press any)"
table:	.word	Ca,Cb,Cc,no,no,no,no,no,Ci
bombBoard:	.word 	0x20:2000
flagspot:	.word	0:2000
	.extern lfsr,word	
	.code	
taps	= 1<<1|1<<2|1<<28|1
	.globl	main

main:
	la	$a0,s
	syscall	$print_string	# loads our seed prompt
	syscall	$read_int
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
make:	mov	$t1,$s1
	mov	$t2,$s2
	addi	$a0,$0,'\f
	syscall	$print_char	# clear screen
	la	$a0,t
	syscall	$print_string	# place title
	mov	$t3,$s2
	li	$a0,0xf0
for1:	syscall	$print_char	#double for loop
	addi	$t2,$t2,-1
	bnez	$t2,for1
	mov	$t2,$t3
	addi	$a0,$0,'\n
	syscall	$print_char
	li	$a0,0xf0
	addi	$t1,$t1,-1
	bnez	$t1,for1	
	
# Function: PLACES THE BOMBS 
# Inputs: NONE
# OutPuts: NONE
	mov	$t4,$s0		# bomb counter
	la	$s3,bombBoard
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
	add	$s4,$t0,$s3	# gets address of point
	lw	$a2,($s4)
	addi	$a3,$0,0xf	# loads bomb
	beq	$a2,$a3,j	# if point is a bomb then jump	
	mov	$a2,$0
	li	$v1,0
	jal	BCoCS	
BCD:	
	beqz	$v1,j		# if a bomb was found cont
	addi	$v1,$v1,'0
	sw	$v1,($s4)
j:	addi	$a0,$a0,1	# iterate col
	bne	$a0,$s2,loop
	li	$a0,0		# restock col
	addi	$a1,$a1,1	# iterate row
	ble	$a1,$s1,loop
	b	pregame

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
	add	$t1,$t0,$s3	# t1 holds address
	addi	$t2,$0,0xf	# check
	lw	$v0,($t1)
	bne	$t2,$v0,cjump
	mov	$v0,$0
	jr	$ra		# return
cjump:	
	sw	$t2,($t1)
	li	$v0,1
	jr	$ra		# return

# Function: COUNTS BOMBS OR CLEAR SPACES (USE FOR SUROUNDING EIGHT SPACES)
# Inputs: $A0 AS COL AND $A1 AS ROW AND A2 WHICH LOAD?
# ( IF A2 = 0 THEN COUNT BOMBS, A2 != 0 THEN CLEAR SPACES FOR GAME )
# OutPuts: $V1 COUNT OF BOMBS
BCoCS:
	beqz	$a2,BC0
	addi	$sp,$sp,-4
	sw	$ra,0($sp)
BC0:	addi	$a0,$a0,1
	addi	$a1,$a1,1
	jal	Boundcheck	# south-east
	bnez	$v0,BC1
	b	BC111
BC1:	beqz	$a2,BC11
	jal	CLEAR8
	b	BC111
BC11:	jal	Load8		# load the space next to current
BC111:	addi	$a0,$a0,-1
	jal	Boundcheck	# south
	bnez	$v0,BC2
	b	BC222
BC2:	beqz	$a2,BC22
	jal	CLEAR8
	b	BC222
BC22:	jal	Load8		# load the space next to current
BC222:	addi	$a0,$a0,-1
	jal	Boundcheck	# south-west
	bnez	$v0,BC3
	b	BC333
BC3:	beqz	$a2,BC33
	jal	CLEAR8
	b	BC333
BC33:	jal	Load8		# load the space next to current
BC333:	addi	$a1,$a1,-1
	jal	Boundcheck	# west
	bnez	$v0,BC4
	b	BC444
BC4:	beqz	$a2,BC44
	jal	CLEAR8
	b	BC444
BC44:	jal	Load8		# load the space next to current
BC444:	addi	$a1,$a1,-1
	jal	Boundcheck	# north-west
	bnez	$v0,BC5
	b	BC555
BC5:	beqz	$a2,BC55
	jal	CLEAR8
	b	BC555
BC55:	jal	Load8		# load the space next to current
BC555:	addi	$a0,$a0,1
	jal	Boundcheck	# north
	bnez	$v0,BC6
	b	BC666
BC6:	beqz	$a2,BC66
	jal	CLEAR8
	b	BC666
BC66:	jal	Load8		# load the space next to current
BC666:	addi	$a0,$a0,1
	jal	Boundcheck	# north-east
	bnez	$v0,BC7
	b	BC777
BC7:	beqz	$a2,BC77
	jal	CLEAR8
	b	BC777
BC77:	jal	Load8		# load the space next to current
BC777:	addi	$a1,$a1,1
	jal	Boundcheck	# east
	bnez	$v0,BC8
	b	BC888
BC8:	beqz	$a2,BC88
	jal	CLEAR8
	b	BC888
BC88:	jal	Load8		# load the space next to current
BC888:	addi	$a0,$a0,-1
	beqz	$a2,BCD
	lw	$ra,0($sp)	
	addiu	$sp,$sp,4
	jr	$ra

# Function: CHECKS THE NEIGHBORING BOX
# Inputs: $A0 AS COL AND $A1 AS ROW
# OutPuts: $V1 COUNT OF BOMBS
CLEAR8:	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	add	$t1,$t0,$s4	# gets address of flag
	lw	$t0,($t1)
	bnez	$t0,cOut	# if flagged or cleared
	syscall	$xy		# " " or # will be printed
	addi	$s5,$s5,-1	# space counter
	li	$t0,1
	sw	$t0,($t1)	# to know it's been clicked on
	sub	$t1,$t1,$s4
	add	$t1,$t1,$s3	# bomb add
	mov	$t2,$a0
	lw	$a0,($t1)
	syscall	$print_char	# prints bomb thing
	mov	$t0,$a0
	mov	$a0,$t2
	li	$t1,0x20
	bne	$t0,$t1,cOut	# check if space
	addi	$sp,$sp,-4
	sw	$ra,0($sp)
	jal	BCoCS		# recursive
	lw	$ra,0($sp)	
	addiu	$sp,$sp,4
	jr	$ra

cOut:	jr	$ra		# return

# Function: CHECKS THE BOUND OF SEARCH
# Inputs: $A0 AS COL AND $A1 AS ROW
# OutPuts: $V0, 0 OUT OF BOUNDS 1 IN BOUNDS	
Boundcheck:
	sltu	$t0,$a0,$s2	# check the bounds of col(x)
	bnez	$t0,B1
	mov	$v0,$0		# v0=0 is outof bounds
	jr	$ra		# return
B1:	addi	$t1,$a1,-1
	sltu	$t0,$t1,$s1 	# check the bounds of row(y)
	bnez	$t0,B2
	mov	$v0,$0		# v0=0 is outof bounds
	jr	$ra		# return
B2:	li	$v0,1		# v0=1 is outof bounds
	jr	$ra		# return

# Function: CHECKS THE NEIGHBORING BOX
# Inputs: $A0 AS COL AND $A1 AS ROW
# OutPuts: $V1 COUNT OF BOMBS
Load8:	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	add	$t1,$t0,$s3	# gets address of point
	lw	$t0,($t1)
	addi	$t2,$0,0xf	# checks for bomb
	beq	$t2,$t0,L8
	jr	$ra		# return
L8:	addi	$v1,$v1,1
# 	sub	$t1,$$t1,$s3
# 	add	$t1,$t1,$s4	# get flag add
# 	lw	$t0,($t1)
# 	li	$t1,0xf0
	jr	$ra		# return
	
# Function: CLEARS THE bombBoard DATA 
# Inputs: NONE
# OutPuts: NONE()
clear:
	mul	$t0,$s1,$s2
	sll	$t1,$s2,2
	add	$t0,$t0,$t1	# finding the points to clear
	add	$t0,$t0,$t1
	la	$t1,bombBoard	
	la	$t2,flagspot
	mov	$v0,$0
	addi	$v1,$0,0x20
cloop:	sw	$v1,($t1)	# clearing out the area
	sw	$v0,($t2)
	addi	$t1,$t1,word
	addi	$t2,$t2,word
	addi	$t0,$t0,-1	# interate
	bnez	$t0,cloop	# loops
	jr	$ra		# return

# Function: SETS UP THE GAME
# Inputs: NONE
# OutPuts: NONE()
pregame:	
	mul	$s5,$s1,$s2	# number spaces left 
	sub	$s5,$s5,$s0
	mov	$s6,$s0		# bomb count for game
	mov	$s7,$s0		# flag count
	jal	printBomb
	la	$s4,flagspot
	mov	$t0,$0		# makes timer to zero
	mtc1	$t0,$f13
	la	$t6,times
	li	$t7,4
	sll	$a0,$t7,4
	add	$a0,$a0,$t6	# timer address
	s.d	$f13,($a0)	# stores timer value
	la	$a0,timer.t4		# hardware address of 
	addi	$a1,$0,4		# 4 byte data
	addi	$a2,$0,1000		# timer interval every 1000 milliseconds
	syscall $IO_write
	mov	$t5,$0

# Function: PRINTS TIMER AND READS MOUSE
# Inputs: NONE
# OutPuts: NONE()
poll:	beqz	$t5,2f
	jal	TimePrint
2:	la	$a0,mouse.flags	# returns mouse click position in v0 and the type of click in v1
	addi	$a1,$0,1
	syscall	$IO_read
	andi	$t0,$v0,mouseFlag.down
	beqz	$t0,3f
	addi	$a0,$a0,mouse.down-mouse.flags
	addi	$a1,$0,4
	syscall	$IO_read
	sw	$v0,mouseDownXY($gp)
	addi	$a0,$a0,word
	addi	$a1,$0,1
	syscall	$IO_read
	sb	$v0,mouseDownButtons($gp)
	b	poll
3:	andi	$t0,$v0,mouseFlag.up
	beqz	$t0,timerCheck
	addi	$a0,$a0,mouse.up-mouse.flags
	addi	$a1,$0,4
	syscall	$IO_read
	lw	$t0,mouseDownXY($gp)
	bne	$t0,$v0,timerCheck
	addi	$a0,$a0,word
	addi	$a1,$0,1
	syscall	$IO_read
	andi	$v0,$v0,mouseButtons.left|mouseButtons.right|mouseButtons.middle
	bnez	$v0,timerCheck
	lw	$v0,mouseDownXY($gp)
	lb	$v1,mouseDownButtons($gp)
	jal	readClick	
timerCheck:
	b	poll

# Function: PRINTS TIMER
# Inputs: NONE
# OutPuts: NONE()
TimePrint:
	la	$a0,timer.flags		
	addi	$a1,$0,1
	syscall $IO_read
	andi	$v0,$v0,16
	blez	$v0,5f			# if(!timer)goto 5;
	la	$a0,timer.t4		# timer hardware address pointer
	syscall	$IO_read		# clear flag
	addi	$a1,$0,0		# position cursor by timer index
	addi	$a0,$0,7
	syscall	$xy
	la	$t6,times
	sll	$a0,$t7,4
	add	$a0,$a0,$t6
	l.d	$f12,($a0)			# pick up elapsed time
	l.d	$f10,8($a0)		# get associated interval
	add.d	$f12,$f12,$f10
	s.d	$f12,($a0)
	syscall	$print_double
	la	$a0,space2
	syscall	$print_string
5:	jr	$ra

# Function: READS CLICKS x
# Inputs: NONE
# OutPuts: NONE()
readClick:
	srl	$t2,$v1,3	# the type of click
	srl	$a1,$v0,16
	andi	$a0,$v0,0x7f	
	mov	$a3,$a0
	syscall	$xy		# the x and y of click
	jal	Boundcheck	# checking if click was in bounds
	bnez	$v0,rc1
	b 	poll		# reads again if out
rc1:	li	$t5,1
	li	$t0,1	
	bne	$t0,$t2,rc2	# checks if left click
	jal	bombAndFlagAdd
	lw	$t0,($v0)
	li	$t1,0xfa
	beq	$t0,$t1,poll	# if clicked on flag then ignore click
	bnez	$t0,rc11	# checks if already clicked on
	li	$t1,1
	sw	$t1,($v0)	# marked first click
	addi	$s5,$s5,-1	# click counter	
	li	$t0,0x20
rc11:	lw	$a0,($v1)	# check if a space
	bne	$a0,$t0,rc111
	mov	$t0,$a3		# saves xy
	mov	$a3,$a0		# saves char
	mov	$a0,$t0		# sets xy
	li	$a2,1
	jal	BCoCS		# recursive
	syscall	$xy
	mov	$a0,$a3		# sets char
rc111:	addi	$t0,$0,0xf
	bne	$t0,$a0,rcD	# checking if clicked on bomb
	syscall	$print_char	
	b	lose		# if so reveal bomb under
rc2:	li	$t0,2
	bne	$t0,$t2,r3	# checks if right click
	jal	bombAndFlagAdd	
	lw	$t0,($v0)
	li	$t1,1
	beq	$t0,$t1,poll	
	beqz	$t0,rc22	# checks the status of flag
	addi	$s7,$s7,1	# where there IS a flag
	sw	$0,($v0)	# unmarking flag
	sub	$v0,$v0,$s4
	add	$v0,$v0,$s3
	lw	$t0,($v0)
	addi	$t1,$0,0xf	# checking if bomb
	bne	$t0,$t1,rc21
	addi	$s6,$s6,1	# if bomb then bomb win counter must go up
rc21:	jal	printBomb	# change bomb counter
	addi	$a0,$0,0xf0
	b	rcD
rc22:	beqz	$s7,poll  	# where there is NO flag                                                         	
	lw	$t0,($v1)
	addi	$t1,$0,0xf
	bne	$t0,$t1,rc23	# checks if bomb
	addi	$s6,$s6,-1
rc23:	addi	$s7,$s7,-1
	jal	printBomb	# change bomb counter
	addi	$t4,$0,0xfa
	jal	bombAndFlagAdd
	sw	$t4,($v0)
	mov	$a0,$t4		# print and store flag
	b	rcD
r3:	li	$t0,3
	bne	$t0,$t2,r33
	b	r333
r33:	li	$t0,4
	bne	$t0,$t2,poll
r333:
	mov	$a2,$0
	li	$v1,0
ChoCL:
	addi	$a0,$a0,1
	addi	$a1,$a1,1
	jal	Boundcheck	# south-east
	bnez	$v0,bc1
	b	bc111
bc1:	beqz	$a2,bc11
	jal	Clear8
	b	bc111
bc11:	jal	Check8		# load the space next to current
bc111:	addi	$a0,$a0,-1
	jal	Boundcheck	# south
	bnez	$v0,bc2
	b	bc222
bc2:	beqz	$a2,bc22
	jal	Clear8
	b	bc222
bc22:	jal	Check8		# load the space next to current
bc222:	addi	$a0,$a0,-1
	jal	Boundcheck	# south-west
	bnez	$v0,bc3
	b	bc333
bc3:	beqz	$a2,bc33
	jal	Clear8
	b	bc333
bc33:	jal	Check8		# load the space next to current
bc333:	addi	$a1,$a1,-1
	jal	Boundcheck	# west
	bnez	$v0,bc4
	b	bc444
bc4:	beqz	$a2,bc44
	jal	Clear8
	b	bc444
bc44:	jal	Check8		# load the space next to current
bc444:	addi	$a1,$a1,-1
	jal	Boundcheck	# north-west
	bnez	$v0,bc5
	b	bc555
bc5:	beqz	$a2,bc55
	jal	Clear8
	b	bc555
bc55:	jal	Check8		# load the space next to current
bc555:	addi	$a0,$a0,1
	jal	Boundcheck	# north
	bnez	$v0,bc6
	b	bc666
bc6:	beqz	$a2,bc66
	jal	Clear8
	b	bc666
bc66:	jal	Check8		# load the space next to current
bc666:	addi	$a0,$a0,1
	jal	Boundcheck	# north-east
	bnez	$v0,bc7
	b	bc777
bc7:	beqz	$a2,bc77
	jal	Clear8
	b	bc777
bc77:	jal	Check8		# load the space next to current
bc777:	addi	$a1,$a1,1
	jal	Boundcheck	# east
	bnez	$v0,bc8
	b	bc888
bc8:	beqz	$a2,bc88
	jal	Clear8
	b	bc888
bc88:	jal	Check8		# load the space next to current
bc888:	addi	$a0,$a0,-1
	bnez	$v1,poll
	li	$a2,1
	li	$v1,1
	b	ChoCL


rcD:	syscall	$print_char
	jal	printBomb
	jal	winCheck
	b	poll

# Function: GETS THE BOM AND FLAG ADDRESS
# Inputs: A1 ROW AND A0 COL
# OutPuts: V0 FLAG ADRESS AND V1 BOMB ADDRESS
bombAndFlagAdd:
	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	add	$v0,$t0,$s4	# flag add
	sub	$v1,$v0,$s4
	add	$v1,$v1,$s3	# bomb add
	jr	$ra

# Function: PRINTS THE BOMB COUTER
# Inputs: S7 BOMBS NOT FLAGGED
# OutPuts: NONE
printBomb:
	mov	$t0,$a0
	mov	$t1,$a1
	addi	$a1,$0,0
	addi	$a0,$0,20
	syscall	$xy		# position cursor by bomb index
	add	$a0,$0,$s7
	syscall	$print_int	# displace bomb counter
	la	$a0,space2
	syscall	$print_string
	addi	$a0,$0,35
	syscall	$xy
	add	$a0,$0,$s5
	syscall	$print_int
	la	$a0,space2
	syscall	$print_string
	mov	$a0,$t0
	mov	$a1,$t1
	syscall	$xy		# sets the old xy back
	jr	$ra


Check8:
	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	add	$t1,$t0,$s3	# gets address of point
	lw	$t0,($t1)
	addi	$t2,$0,0xf	# checks for bomb
	bne	$t2,$t0,C8
	addi	$v1,$v1,-1
C8:	sub	$t1,$t1,$s3
	add	$t1,$t1,$s4	# flag add
	lw	$t0,($t1)
	addi	$t2,$0,0xfa	# checks for flag
	bne	$t2,$t0,Ch8
	addi	$v1,$v1,1
Ch8:	jr	$ra


Clear8:	
	addi	$t0,$s2,1
	mul	$t0,$t0,$a1
	add	$t0,$t0,$a0
	sll	$t0,$t0,2
	add	$t1,$t0,$s4	# gets address of flag
	lw	$t0,($t1)	
	bnez	$t0,Cout	# if flagged or cleared
	syscall	$xy		# " " or # will be printed
	addi	$s5,$s5,-1	# space counter
	li	$t0,1
	sw	$t0,($t1)	# to know it's been clicked on
	sub	$t1,$t1,$s4
	add	$t1,$t1,$s3	# bomb add
	mov	$t2,$a0
	lw	$a0,($t1)
	syscall	$print_char	# prints bomb thing
	mov	$t0,$a0
	mov	$a0,$t2
	li	$t1,0x20
	bne	$t0,$t1,Cout	# check if space
	addi	$sp,$sp,-4
	sw	$ra,0($sp)
	jal	BCoCS		# recursive
	lw	$ra,0($sp)	
	addiu	$sp,$sp,4
Cout:	jr	$ra		# return


# Function: CHECKS THE WIN CONDITIONS
# Inputs: S5 SPACES LEFT UNCLICKED AND S6 FLAGGED BOMBS 
# OutPuts: NONE()
winCheck:	
	bnez	$s6,w1		# checks if win my flags
	b	winner
w1:	bnez	$s5,w2		# checks is win by space
	b	winner
w2:	jr	$ra


# Function: PRINTS WIN MESSAGE
# Inputs: NONE
# OutPuts: NONE()
winner:	li	$a0,57
	li	$a1,0
	syscall	$xy		# position message
	la	$a0,w
	syscall	$print_string	# print win message
	jal	Record
	syscall	$read_char
	la	$a0,timer.t4		# hardware address of 
	addi	$a1,$0,4		# 4 byte data
	addi	$a2,$0,0		# timer interval every 1000 milliseconds
	syscall $IO_write
	jal	clear
	b	make		# read char to reset board

# Function: PRINTS LOSE MESSAGE
# Inputs: NONE
# OutPuts: NONE()	
lose:	li	$a0,57
	li	$a1,0
	syscall	$xy		# position message
	la	$a0,l
	syscall	$print_string	# print lose message
	jal	Record
	syscall	$read_char
	la	$a0,timer.t4		# hardware address of 
	addi	$a1,$0,4		# 4 byte data
	addi	$a2,$0,0		# timer interval every 1000 milliseconds
	syscall $IO_write
	jal	clear
	b	make		# read char to reset board

# Function: KEEPS TRACK OF RECORDS OF GAMES
# Inputs: NONE
# OutPuts: NONE()
Record:				# to save a resigter, check bom, row, adn col
	li	$t0,10		# to make sure it's not custom
	bne	$s0,$t0,R1	# check the bomb
	li	$t0,9
	bne	$s1,$t0,Rout	# check the row
	bne	$s2,$t0,Rout	# check the col
	li	$t0,1
	b	Rin
R1:	li	$t0,40
	bne	$s0,$t0,R2	# check the bomb
	li	$t0,16
	bne	$s1,$t0,Rout	# check the row
	bne	$s2,$t0,Rout	# check the col
	li	$t0,2
	b	Rin
R2:	li	$t0,99
	bne	$s0,$t0,Rout	# check the bomb
	li	$t0,16
	bne	$s1,$t0,Rout	# check the row
	li	$t0,30
	bne	$s2,$t0,Rout	# check the col
	li	$t0,3
Rin:	

Rout:	jr	$ra

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
space2:	.asciiz "  "
	.align 3
times:	.double	0,0
	.double 0,0.125
	.double 0,0.25
	.double	0,0.5
	.double	0,1.0
	.globl main   
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
	.code
#----------------------------------------------------------------------------
	.data
record:	.struct
cols:	.byte	0
rows:	.byte	0
mines:	.half	0
wins:	.word	0
losses:	.word	0
best:	.word	0
	.data
rec1:	.space	record