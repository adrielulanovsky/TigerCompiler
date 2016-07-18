.data


L2: 
	.long 3

	.string "str"

L3: 
	.long 1

	.string " "



L4: 
	.long 4

	.string "str2"


.text

.type .do_nothing17_L0, @function
.do_nothing17_L0:
	pushl %ebp 
	movl %esp, %ebp 
	subl $4, %esp
L6:
	movl %esi, -4(%ebp)
	movl $1, %eax
	movl 12(%ebp), %esi
	addl %eax, %esi
	#guardando caller
	pushl %esi
	movl 8(%ebp), %eax
	pushl %eax
	call .do_nothing210_L1
	addl $8, %esp
	#recuperando caller
	movl $0, %eax
	movl -4(%ebp), %esi
	jmp L5
L5:
	addl $4, %esp
	leave
	ret


.type .do_nothing210_L1, @function
.do_nothing210_L1:
	pushl %ebp 
	movl %esp, %ebp 
	subl $4, %esp
L8:
	movl %esi, -4(%ebp)
	movl 12(%ebp), %esi
	#guardando caller
	pushl $L2
	pushl %esi
	movl 8(%ebp), %eax
	pushl %eax
	call .do_nothing17_L0
	addl $12, %esp
	#recuperando caller
	movl $L3, %eax
	movl -4(%ebp), %esi
	jmp L7
L7:
	addl $4, %esp
	leave
	ret

.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L10:
	#guardando caller
	pushl $L4
	pushl $0
	pushl %ebp
	call .do_nothing17_L0
	addl $12, %esp
	#recuperando caller
	jmp L9
L9:
	addl $0, %esp
	leave
	ret

