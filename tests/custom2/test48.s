.data





.text

.type .g7_L0, @function
.g7_L0:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L3:
	movl 12(%ebp), %eax
	jmp L2
L2:
	addl $0, %esp
	leave
	ret


.type .g9_L1, @function
.g9_L1:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L5:
	movl 12(%ebp), %eax
	jmp L4
L4:
	addl $0, %esp
	leave
	ret

.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L7:
	movl $0, %eax
	jmp L6
L6:
	addl $0, %esp
	leave
	ret

