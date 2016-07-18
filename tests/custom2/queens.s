.data
L4: 
	.long 2

	.string " O"

L5: 
	.long 2

	.string " ."

L11: 
	.long 1

	.string "\x0a"

L14: 
	.long 1

	.string "\x0a"






.text

.type .printboard20_L0, @function
.printboard20_L0:
	pushl %ebp 
	movl %esp, %ebp 
	subl $24, %esp
L38:
	movl %ebx, -20(%ebp)
	movl %edi, -16(%ebp)
	movl %esi, -12(%ebp)
	movl $0, -4(%ebp)
	movl $1, %ecx
	movl 8(%ebp), %edx
	movl $-4, %eax
	addl %edx, %eax
	movl (%eax), %eax
	subl %ecx, %eax
	movl %eax, -24(%ebp)
	movl -4(%ebp), %edx
	movl -24(%ebp), %eax
	cmpl %eax, %edx
	jle L13
L2:
	#guardando caller
	pushl $L14
	call print
	addl $4, %esp
	#recuperando caller
	movl -20(%ebp), %ebx
	movl -16(%ebp), %edi
	movl -12(%ebp), %esi
	jmp L37
L13:
	movl $0, -8(%ebp)
	movl $1, %edx
	movl 8(%ebp), %eax
	movl $-4, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	subl %edx, %ebx
	movl -8(%ebp), %eax
	cmpl %ebx, %eax
	jle L10
L3:
	#guardando caller
	pushl $L11
	call print
	addl $4, %esp
	#recuperando caller
	movl -4(%ebp), %edx
	movl -24(%ebp), %eax
	cmpl %eax, %edx
	je L2
L12:
	movl $1, %edx
	movl -4(%ebp), %eax
	addl %edx, %eax
	movl $-4, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	jmp L13
L10:
	movl 8(%ebp), %eax
	movl $-12, %edi
	addl %eax, %edi
	movl (%edi), %edi
	movl -4(%ebp), %esi
	#guardando caller
	pushl %esi
	pushl %edi
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl -8(%ebp), %eax
	movl $4, %edx
	imul %edx, %esi
	addl %esi, %edi
	movl (%edi), %edi
	cmpl %eax, %edi
	je L6
L7:
	movl $L5, %edi
L8:
	#guardando caller
	pushl %edi
	call print
	addl $4, %esp
	#recuperando caller
	movl -8(%ebp), %eax
	cmpl %ebx, %eax
	je L3
L9:
	movl $1, %edx
	movl -8(%ebp), %eax
	addl %edx, %eax
	movl $-8, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	jmp L10
L6:
	movl $L4, %edi
	jmp L8
L37:
	addl $24, %esp
	leave
	ret


.type .try32_L1, @function
.try32_L1:
	pushl %ebp 
	movl %esp, %ebp 
	subl $20, %esp
L40:
	movl %ebx, -16(%ebp)
	movl %edi, -12(%ebp)
	movl %esi, -8(%ebp)
	movl 8(%ebp), %edx
	movl $-4, %eax
	addl %edx, %eax
	movl (%eax), %eax
	movl 12(%ebp), %edx
	cmpl %eax, %edx
	je L34
L35:
	movl $0, -4(%ebp)
	movl $1, %ecx
	movl 8(%ebp), %edx
	movl $-4, %eax
	addl %edx, %eax
	movl (%eax), %eax
	subl %ecx, %eax
	movl %eax, -20(%ebp)
	movl -4(%ebp), %edx
	movl -20(%ebp), %eax
	cmpl %eax, %edx
	jle L33
L15:
L36:
	movl -16(%ebp), %ebx
	movl -12(%ebp), %edi
	movl -8(%ebp), %esi
	jmp L39
L34:
	#guardando caller
	movl 8(%ebp), %eax
	pushl %eax
	call .printboard20_L0
	addl $4, %esp
	#recuperando caller
	jmp L36
L33:
	movl 8(%ebp), %eax
	movl $-8, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl -4(%ebp), %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl (%ebx), %ebx
	cmpl $0, %ebx
	je L20
L21:
	movl $0, %ebx
L22:
	cmpl $0, %ebx
	jne L27
L28:
	movl $0, %ebx
L29:
	cmpl $0, %ebx
	jne L30
L31:
	movl -4(%ebp), %edx
	movl -20(%ebp), %eax
	cmpl %eax, %edx
	je L15
L32:
	movl $1, %edx
	movl -4(%ebp), %eax
	addl %edx, %eax
	movl $-4, %edx
	addl %ebp, %edx
	movl %eax, (%edx)
	jmp L33
L20:
	movl $1, %ebx
	movl 8(%ebp), %eax
	movl $-16, %edi
	addl %eax, %edi
	movl (%edi), %edi
	movl 12(%ebp), %eax
	movl -4(%ebp), %esi
	addl %eax, %esi
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
	cmpl $0, %edi
	je L18
L19:
	movl $0, %ebx
L18:
	jmp L22
L27:
	movl $1, %ebx
	movl 8(%ebp), %eax
	movl $-20, %edi
	addl %eax, %edi
	movl (%edi), %edi
	movl 12(%ebp), %eax
	movl $7, %edx
	movl -4(%ebp), %esi
	addl %edx, %esi
	subl %eax, %esi
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
	cmpl $0, %edi
	je L25
L26:
	movl $0, %ebx
L25:
	jmp L29
L30:
	movl 8(%ebp), %eax
	movl $-8, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl -4(%ebp), %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $1, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl 8(%ebp), %eax
	movl $-16, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl 12(%ebp), %eax
	movl -4(%ebp), %edi
	addl %eax, %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $1, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl 8(%ebp), %eax
	movl $-20, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl 12(%ebp), %edx
	movl $7, %eax
	movl -4(%ebp), %edi
	addl %eax, %edi
	subl %edx, %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $1, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl 8(%ebp), %eax
	movl $-12, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl 12(%ebp), %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $-4, %eax
	addl %ebp, %eax
	movl (%eax), %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl $1, %eax
	movl 12(%ebp), %ebx
	addl %eax, %ebx
	#guardando caller
	pushl %ebx
	movl 8(%ebp), %eax
	pushl %eax
	call .try32_L1
	addl $8, %esp
	#recuperando caller
	movl 8(%ebp), %eax
	movl $-8, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl -4(%ebp), %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $0, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl 8(%ebp), %eax
	movl $-16, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl 12(%ebp), %eax
	movl -4(%ebp), %edi
	addl %eax, %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $0, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	movl 8(%ebp), %eax
	movl $-20, %ebx
	addl %eax, %ebx
	movl (%ebx), %ebx
	movl 12(%ebp), %edx
	movl $7, %eax
	movl -4(%ebp), %edi
	addl %eax, %edi
	subl %edx, %edi
	#guardando caller
	pushl %edi
	pushl %ebx
	call _checkIndexArray
	addl $8, %esp
	#recuperando caller
	movl $0, %edx
	movl $4, %eax
	imul %eax, %edi
	addl %edi, %ebx
	movl %edx, (%ebx)
	jmp L31
L39:
	addl $20, %esp
	leave
	ret

.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $32, %esp
L42:
	movl %ebx, -32(%ebp)
	movl %edi, -28(%ebp)
	movl %esi, -24(%ebp)
	movl $8, -4(%ebp)
	movl $-8, %ebx
	addl %ebp, %ebx
	#guardando caller
	pushl $0
	movl -4(%ebp), %eax
	pushl %eax
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl $-12, %ebx
	addl %ebp, %ebx
	#guardando caller
	pushl $0
	movl -4(%ebp), %eax
	pushl %eax
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl $-16, %ebx
	addl %ebp, %ebx
	#guardando caller
	pushl $0
	movl $1, %edi
	movl -4(%ebp), %eax
	movl -4(%ebp), %esi
	addl %eax, %esi
	subl %edi, %esi
	pushl %esi
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%ebx)
	movl $-20, %ebx
	addl %ebp, %ebx
	#guardando caller
	pushl $0
	movl $1, %edi
	movl -4(%ebp), %eax
	movl -4(%ebp), %esi
	addl %eax, %esi
	subl %edi, %esi
	pushl %esi
	call _initArray
	addl $8, %esp
	#recuperando caller
	movl %eax, (%ebx)
	#guardando caller
	pushl $0
	pushl %ebp
	call .try32_L1
	addl $8, %esp
	#recuperando caller
	movl $0, %eax
	movl -32(%ebp), %ebx
	movl -28(%ebp), %edi
	movl -24(%ebp), %esi
	jmp L41
L41:
	addl $32, %esp
	leave
	ret

