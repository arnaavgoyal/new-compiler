foo:
	mov	rax, 0
	ret	
bar:
	mov	[rbp 0], [rbp 0]
	call	foo
	ret	
main:
	mov	1, [rbp 4]
	mov	[rbp 0], 1
	call	bar
	mov	rax, 3
	ret	
