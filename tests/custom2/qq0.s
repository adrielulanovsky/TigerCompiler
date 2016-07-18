.data
L0: 
	.long 4

	.string "hola"

L3: 
	.long 1

	.string "\x0a"


.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $20, %esp
L9:
	movl %edi, -20(%ebp)
	movl %esi, -16(%ebp)
	movl $-4, %edi
	addl %ebp, %edi
	#guardando caller
	pushl $L0
	pushl $10
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, %esi
	#guardando caller
	pushl %esi
	pushl $10
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%edi)
	movl $0, -8(%ebp)
L7:
	movl -8(%ebp), %eax
	cmpl $9, %eax
	jg L1
L6:
	movl $0, -12(%ebp)
L5:
	movl -12(%ebp), %eax
	cmpl $9, %eax
	jg L2
L4:
	movl -4(%ebp), %esi
	movl -8(%ebp), %edi
	#guardando caller
	pushl %edi
	pushl %esi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %esi
	movl (%esi), %esi
	movl -12(%ebp), %edi
	#guardando caller
	pushl %edi
	pushl %esi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %esi
	movl (%esi), %esi
	#guardando caller
	pushl %esi
	call print
	addl $4, %esp
	#recuperando caller
	#guardando caller
	pushl $L3
	call print
	addl $4, %esp
	#recuperando caller
	movl $1, %edx
	movl -12(%ebp), %eax
	addl %edx, %eax
	movl $-12, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	jmp L5
L2:
	movl $1, %edx
	movl -8(%ebp), %eax
	addl %edx, %eax
	movl $-8, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	jmp L7
L1:
	movl $0, %eax
	movl -20(%ebp), %edi
	movl -16(%ebp), %esi
	jmp L8
L8:
	addl $20, %esp
	leave
	ret

