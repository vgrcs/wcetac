	.file	"fibcall.c"
	.text
	.align	2
	.global	fib
	.type	fib, %function
fib:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #24
	str	r0, [fp, #-36]
	mov	r3, #1
	str	r3, [fp, #-28]
	mov	r3, #0
	str	r3, [fp, #-24]
	mov	r3, #2
	str	r3, [fp, #-32]
	b	.L2
.L3:
	ldr	r3, [fp, #-28]
	str	r3, [fp, #-20]
	ldr	r2, [fp, #-28]
	ldr	r3, [fp, #-24]
	add	r3, r2, r3
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-20]
	str	r3, [fp, #-24]
	ldr	r3, [fp, #-32]
	add	r3, r3, #1
	str	r3, [fp, #-32]
.L2:
	ldr	r3, [fp, #-32]
	cmp	r3, #30
	bgt	.L4
	ldr	r2, [fp, #-32]
	ldr	r3, [fp, #-36]
	cmp	r2, r3
	ble	.L3
.L4:
	ldr	r3, [fp, #-28]
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	fib, .-fib
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 4
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #4
	mov	r3, #5
	str	r3, [fp, #-16]
	ldr	r0, [fp, #-16]
	bl	fib
	ldr	r3, [fp, #-16]
	mov	r0, r3
	ldmfd	sp, {r3, fp, sp, pc}
	.size	main, .-main
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
