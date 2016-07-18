.data



.text

.type .g6_L0, @function
.g6_L0:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L2:
	movl 12(%ebp), %eax
	jmp L1
L1:
	addl $0, %esp
	leave
	ret

.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $4, %esp
L4:
	movl $0, -4(%ebp)
	#guardando caller
	pushl $2
	pushl %ebp
	call .g6_L0
	addl $8, %esp
	#recuperando caller
	jmp L3
L3:
	addl $4, %esp
	leave
	ret

