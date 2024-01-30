	.intel_syntax noprefix
	.globl foo
	.globl bar
	.globl my_main
foo:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 0
	mov	rax, [rbp + 16]
	mov	rsp, rbp
	pop	rbp
	ret
bar:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 4
	mov	[rbp - 4], 14
	call	foo
	mov	rsp, rbp
	pop	rbp
	ret
my_main:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 8
	mov	rax, [rbp + 20]
	mov	[rbp - 8], rax
	call	bar
	mov	rsp, rbp
	pop	rbp
	ret
