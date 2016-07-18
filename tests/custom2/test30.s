.data

.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $12, %esp
L1:
	movl %edi, -12(%ebp)
	movl %esi, -8(%ebp)
	movl $-4, %edi
	addl %ebp, %edi
	#guardando caller
	pushl $42
	pushl $10
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%edi)
	movl -4(%ebp), %edi
	movl $2, %esi
	#guardando caller
	pushl %esi
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $4, %eax
	imul %eax, %esi
	addl %esi, %edi
	movl (%edi), %edi
	movl %edi, %eax
	movl -12(%ebp), %edi
	movl -8(%ebp), %esi
	jmp L0
L0:
	addl $12, %esp
	leave
	ret

