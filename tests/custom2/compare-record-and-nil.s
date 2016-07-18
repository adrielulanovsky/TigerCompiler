.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $8, %esp
L5:
	movl %ebx, %edx
	movl %edi, %ecx
	movl %esi, -8(%ebp)
	movl $0, -4(%ebp)
	movl -4(%ebp), %eax
	cmpl $0, %eax
	je L0
L1:
	movl $1, %eax
	movl -4(%ebp), %ebx
	cmpl $0, %ebx
	jne L2
L3:
	movl $0, %eax
L2:
	movl %edx, %ebx
	movl %ecx, %edi
	movl -8(%ebp), %esi
	jmp L4
L0:
	jmp L1
L4:
	addl $8, %esp
	leave
	ret

