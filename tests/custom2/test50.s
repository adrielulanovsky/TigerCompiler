.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $4, %esp
L6:
	movl %ebx, %edx
	movl %edi, %ecx
	movl %esi, -4(%ebp)
L3:
L4:
L1:
L0:
	movl $0, %eax
	movl %edx, %ebx
	movl %ecx, %edi
	movl -4(%ebp), %esi
	jmp L5
L2:
	jmp L3
L5:
	addl $4, %esp
	leave
	ret

