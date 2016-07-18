.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L1:
	movl $0, %eax
	jmp L0
L0:
	addl $0, %esp
	leave
	ret

