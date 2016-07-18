.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $4, %esp
L4:
	movl %ebx, %edx
	movl %edi, %ecx
	movl %esi, -4(%ebp)
	jmp L1
L1:
	movl $40, %eax
L2:
	movl %edx, %ebx
	movl %ecx, %edi
	movl -4(%ebp), %esi
	jmp L3
L0:
	movl $30, %eax
	jmp L2
L3:
	addl $4, %esp
	leave
	ret

