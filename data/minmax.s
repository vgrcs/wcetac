	.file	"minmax.c"
	.text
	.align	2
	.global	swap
	.type	swap, %function
swap:
	@ args = 0, pretend = 0, frame = 12
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #12
	str	r0, [fp, #-20]
	str	r1, [fp, #-24]
	ldr	r3, [fp, #-20]
	ldr	r3, [r3, #0]
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-24]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-20]
	str	r2, [r3, #0]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-16]
	str	r3, [r2, #0]
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	swap, .-swap
	.align	2
	.global	min
	.type	min, %function
min:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #16
	str	r0, [fp, #-20]
	str	r1, [fp, #-24]
	str	r2, [fp, #-28]
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-24]
	cmp	r2, r3
	bgt	.L4
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-28]
	cmp	r2, r3
	bgt	.L6
	ldr	r3, [fp, #-20]
	str	r3, [fp, #-16]
	b	.L9
.L6:
	ldr	r3, [fp, #-28]
	str	r3, [fp, #-16]
	b	.L9
.L4:
	ldr	r3, [fp, #-24]
	ldr	r2, [fp, #-28]
	cmp	r3, r2
	movge	r3, r2
	str	r3, [fp, #-16]
.L9:
	ldr	r3, [fp, #-16]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	min, .-min
	.align	2
	.global	max
	.type	max, %function
max:
	@ args = 0, pretend = 0, frame = 12
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #12
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	cmp	r2, r3
	bgt	.L12
	sub	r3, fp, #16
	sub	r2, fp, #20
	mov	r0, r3
	mov	r1, r2
	bl	swap
.L12:
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-24]
	cmp	r2, r3
	bgt	.L14
	sub	r3, fp, #16
	sub	r2, fp, #24
	mov	r0, r3
	mov	r1, r2
	bl	swap
.L14:
	ldr	r3, [fp, #-16]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	max, .-max
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #16
	mov	r3, #10
	str	r3, [fp, #-20]
	mov	r3, #2
	str	r3, [fp, #-24]
	mov	r3, #1
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-24]
	cmp	r2, r3
	bgt	.L18
	sub	r3, fp, #20
	sub	r2, fp, #24
	mov	r0, r3
	mov	r1, r2
	bl	swap
	b	.L20
.L18:
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-16]
	cmp	r2, r3
	bgt	.L21
	ldr	r3, [fp, #-20]
	ldr	r2, [fp, #-24]
	mov	r0, r3
	mov	r1, r2
	ldr	r2, [fp, #-16]
	bl	min
	mov	r2, r0
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	str	r3, [fp, #-20]
	b	.L20
.L21:
	ldr	r3, [fp, #-24]
	ldr	r2, [fp, #-20]
	ldr	r0, [fp, #-16]
	mov	r1, r3
	bl	max
	mov	r2, r0
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-16]
.L20:
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-16]
	cmp	r2, r3
	bgt	.L23
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-16]
	add	r2, r2, r3
	str	r2, [fp, #-28]
	b	.L25
.L23:
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-16]
	rsb	r2, r3, r2
	str	r2, [fp, #-28]
.L25:
	ldr	r3, [fp, #-28]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	main, .-main
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
