.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $8, %esp
L1:
	movl %esi, -8(%ebp)
	movl $-4, %esi
	addl %ebp, %esi
	#guardando caller
	pushl $0
	pushl $10
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%esi)
	movl -4(%ebp), %eax
	movl $0, %eax
	movl -8(%ebp), %esi
	jmp L0
L0:
	addl $8, %esp
	leave
	ret

