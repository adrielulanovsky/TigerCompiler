.data
L0: 
	.long 6

	.string "Nobody"

L1: 
	.long 8

	.string "Somebody"


.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $16, %esp
L3:
	movl %ebx, -16(%ebp)
	movl %edi, -12(%ebp)
	movl %esi, -8(%ebp)
	movl $-4, %ebx
	addl %ebp, %ebx
	movl $L0, %edi
	movl $1000, %esi
	#guardando caller
	pushl %esi
	pushl %edi
	pushl $2
	call _allocRecord
	addl $12, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl -4(%ebp), %ebx
	#guardando caller
	pushl %ebx
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $L1, %ecx
	movl $4, %edx
	movl $0, %eax
	imul %edx, %eax
	addl %eax, %ebx
	movl %ecx, (%ebx)
	movl -4(%ebp), %eax
	movl $0, %eax
	movl -16(%ebp), %ebx
	movl -12(%ebp), %edi
	movl -8(%ebp), %esi
	jmp L2
L2:
	addl $16, %esp
	leave
	ret

