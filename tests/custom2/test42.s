.data
L0: 
	.long 5

	.string "aname"

L1: 
	.long 9

	.string "somewhere"

L2: 
	.long 0

	.string ""

L3: 
	.long 7

	.string "Kapoios"

L4: 
	.long 5

	.string "Kapou"

L5: 
	.long 5

	.string "Allos"

L6: 
	.long 4

	.string "kati"

L7: 
	.long 3

	.string "sfd"

L8: 
	.long 3

	.string "sdf"


.text
.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $48, %esp
L10:
	movl %ebx, -32(%ebp)
	movl %edi, -28(%ebp)
	movl %esi, -24(%ebp)
	movl $-4, %ebx
	addl %ebp, %ebx
	#guardando caller
	pushl $0
	pushl $10
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl $-8, %eax
	addl %ebp, %eax
	movl %eax, -44(%ebp)
	movl $L0, %eax
	movl %eax, -48(%ebp)
	movl $L1, %ebx
	movl $0, %edi
	movl $0, %esi
	#guardando caller
	pushl %esi
	pushl %edi
	pushl %ebx
	movl -48(%ebp), %eax
	pushl %eax
	pushl $4
	call _allocRecord
	addl $20, %esp
	#recuperando caller
	movl %eax, %ebx
	#guardando caller
	pushl %ebx
	pushl $5
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl -44(%ebp), %edx
	movl %eax, (%edx)
	movl $-12, %ebx
	addl %ebp, %ebx
	#guardando caller
	pushl $L2
	pushl $100
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl $-16, %eax
	addl %ebp, %eax
	movl %eax, -36(%ebp)
	movl $L3, %eax
	movl %eax, -40(%ebp)
	movl $L4, %ebx
	movl $2432, %edi
	movl $44, %esi
	#guardando caller
	pushl %esi
	pushl %edi
	pushl %ebx
	movl -40(%ebp), %eax
	pushl %eax
	pushl $4
	call _allocRecord
	addl $20, %esp
	#recuperando caller
	movl -36(%ebp), %edx
	movl %eax, (%edx)
	movl $-20, %ebx
	addl %ebp, %ebx
	movl $L5, %edi
	#guardando caller
	pushl $1900
	pushl $3
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, %esi
	#guardando caller
	pushl %esi
	pushl %edi
	pushl $2
	call _allocRecord
	addl $12, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl -4(%ebp), %edi
	movl $0, %ebx
	#guardando caller
	pushl %ebx
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $1, %edx
	movl $4, %eax
	imul %eax, %ebx
	addl %ebx, %edi
	movl %edx, (%edi)
	movl -4(%ebp), %edi
	movl $9, %ebx
	#guardando caller
	pushl %ebx
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $3, %edx
	movl $4, %eax
	imul %eax, %ebx
	addl %ebx, %edi
	movl %edx, (%edi)
	movl -8(%ebp), %edi
	movl $3, %ebx
	#guardando caller
	pushl %ebx
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $4, %eax
	imul %eax, %ebx
	addl %ebx, %edi
	movl (%edi), %edi
	#guardando caller
	pushl %edi
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $L6, %ecx
	movl $4, %edx
	movl $0, %eax
	imul %edx, %eax
	addl %eax, %edi
	movl %ecx, (%edi)
	movl -8(%ebp), %edi
	movl $1, %ebx
	#guardando caller
	pushl %ebx
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $4, %eax
	imul %eax, %ebx
	addl %ebx, %edi
	movl (%edi), %edi
	#guardando caller
	pushl %edi
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $23, %ecx
	movl $4, %edx
	movl $3, %eax
	imul %edx, %eax
	addl %eax, %edi
	movl %ecx, (%edi)
	movl -12(%ebp), %edi
	movl $34, %ebx
	#guardando caller
	pushl %ebx
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $L7, %edx
	movl $4, %eax
	imul %eax, %ebx
	addl %ebx, %edi
	movl %edx, (%edi)
	movl -16(%ebp), %ebx
	#guardando caller
	pushl %ebx
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $L8, %ecx
	movl $4, %edx
	movl $0, %eax
	imul %edx, %eax
	addl %eax, %ebx
	movl %ecx, (%ebx)
	movl -20(%ebp), %ebx
	#guardando caller
	pushl %ebx
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $4, %edx
	movl $1, %eax
	imul %edx, %eax
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl $0, %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $2323, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl -20(%ebp), %ebx
	#guardando caller
	pushl %ebx
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $4, %edx
	movl $1, %eax
	imul %edx, %eax
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl $2, %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $2323, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl $0, %eax
	movl -32(%ebp), %ebx
	movl -28(%ebp), %edi
	movl -24(%ebp), %esi
	jmp L9
L9:
	addl $48, %esp
	leave
	ret

