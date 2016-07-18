.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $8, %esp
L4:
	movl $0, -4(%ebp)
	movl $0, -8(%ebp)
L2:
	movl -8(%ebp), %eax
	cmpl $100, %eax
	jg L0
L1:
	movl $1, %edx
	movl -4(%ebp), %eax
	addl %edx, %eax
	movl $-4, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	movl $1, %edx
	movl -8(%ebp), %eax
	addl %edx, %eax
	movl $-8, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	jmp L2
L0:
	movl -4(%ebp), %eax
	jmp L3
L3:
	addl $8, %esp
	leave
	ret

