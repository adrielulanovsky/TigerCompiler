.data
L5: 
	.long 1

	.string "A"

L6: 
	.long 1

	.string "\x0a"












.text

.type .f59_L4, @function
.f59_L4:
	pushl %ebp 
	movl %esp, %ebp 
	subl $16, %esp
L8:
	movl %ebx, -12(%ebp)
	movl %edi, -8(%ebp)
	movl %esi, -4(%ebp)
	#guardando caller
	pushl $L5
	call ord
	addl $4, %esp
	#recuperando caller
	movl %eax, %ebx
	movl 12(%ebp), %eax
	movl %eax, -16(%ebp)
	movl 8(%ebp), %edx
	movl $12, %eax
	addl %edx, %eax
	movl (%eax), %eax
	movl 8(%ebp), %ecx
	movl $8, %edx
	addl %ecx, %edx
	movl (%edx), %edx
	movl $12, %ecx
	addl %edx, %ecx
	movl (%ecx), %ecx
	movl 8(%ebp), %edi
	movl $8, %edx
	addl %edi, %edx
	movl (%edx), %edx
	movl $8, %edi
	addl %edx, %edi
	movl (%edi), %edi
	movl $12, %edx
	addl %edi, %edx
	movl (%edx), %edx
	movl 8(%ebp), %esi
	movl $8, %edi
	addl %esi, %edi
	movl (%edi), %edi
	movl $8, %esi
	addl %edi, %esi
	movl (%esi), %esi
	movl $8, %edi
	addl %esi, %edi
	movl (%edi), %edi
	movl $12, %esi
	addl %edi, %esi
	movl (%esi), %esi
	addl %esi, %ebx
	addl %edx, %ebx
	addl %ecx, %ebx
	addl %eax, %ebx
	movl -16(%ebp), %eax
	addl %eax, %ebx
	#guardando caller
	pushl %ebx
	call chr
	addl $4, %esp
	#recuperando caller
	movl %eax, %ebx
	#guardando caller
	pushl %ebx
	call print
	addl $4, %esp
	#recuperando caller
	#guardando caller
	pushl $L6
	call print
	addl $4, %esp
	#recuperando caller
	movl -12(%ebp), %ebx
	movl -8(%ebp), %edi
	movl -4(%ebp), %esi
	jmp L7
L7:
	addl $16, %esp
	leave
	ret


.type .f410_L3, @function
.f410_L3:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L10:
	#guardando caller
	pushl $1
	pushl %ebp
	call .f59_L4
	addl $8, %esp
	#recuperando caller
	jmp L9
L9:
	addl $0, %esp
	leave
	ret


.type .f311_L2, @function
.f311_L2:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L12:
	#guardando caller
	pushl $2
	pushl %ebp
	call .f410_L3
	addl $8, %esp
	#recuperando caller
	jmp L11
L11:
	addl $0, %esp
	leave
	ret


.type .f212_L1, @function
.f212_L1:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L14:
	#guardando caller
	pushl $3
	pushl %ebp
	call .f311_L2
	addl $8, %esp
	#recuperando caller
	jmp L13
L13:
	addl $0, %esp
	leave
	ret


.type .f113_L0, @function
.f113_L0:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L16:
	#guardando caller
	pushl $4
	pushl %ebp
	call .f212_L1
	addl $8, %esp
	#recuperando caller
	jmp L15
L15:
	addl $0, %esp
	leave
	ret

.globl _tigermain
.type _tigermain, @function
_tigermain:
	pushl %ebp 
	movl %esp, %ebp 
	subl $0, %esp
L18:
	#guardando caller
	pushl $5
	pushl %ebp
	call .f113_L0
	addl $8, %esp
	#recuperando caller
	movl $0, %eax
	jmp L17
L17:
	addl $0, %esp
	leave
	ret

