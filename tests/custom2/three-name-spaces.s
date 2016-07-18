.data



.text

.type .a4_L0, @function
.a4_L0:
	pushl %ebp 
	movl %esp, %ebp 
	subl $4, %esp
L2:
	movl %esi, -4(%ebp)
	movl 12(%ebp), %esi
	#guardando caller
	pushl %esi
	call _checkNil
	addl $4, %esp
	#recuperando caller
	movl $4, %edx
	movl $0, %eax
	imul %edx, %eax
	addl %eax, %esi
	movl (%esi), %esi
	#guardando caller
	pushl %esi
	pushl $1
	call _allocRecord
	addl $8, %esp
	#recuperando caller
	movl -4(%ebp), %esi
	jmp L1
L1:
	addl $4, %esp
	leave
	ret

.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $8, %esp
L4:
	movl %esi, -8(%ebp)
	movl $0, -4(%ebp)
	movl $0, %esi
	#guardando caller
	pushl %esi
	pushl $1
	call _allocRecord
	addl $8, %esp
	#recuperando caller
	movl %eax, %esi
	#guardando caller
	pushl %esi
	pushl %ebp
	call .a4_L0
	addl $8, %esp
	#recuperando caller
	movl $0, %eax
	movl -8(%ebp), %esi
	jmp L3
L3:
	addl $8, %esp
	leave
	ret

