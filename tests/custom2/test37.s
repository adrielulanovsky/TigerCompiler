.data
L0: 
	.long 1

	.string " "


.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $8, %esp
L2:
	movl $0, -4(%ebp)
	movl $L0, %edx
	movl $-8, %eax
	addl %ebp, %eax
	movl %edx, (%eax)
	movl $0, %eax
	jmp L1
L1:
	addl $8, %esp
	leave
	ret

