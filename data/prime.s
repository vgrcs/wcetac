	.file	"prime.c"
	.global	__umodsi3
	.text
	.align	2
	.global	divides
	.type	divides, %function
divides:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #8
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	ldr	r3, [fp, #-20]
	mov	r0, r3
	ldr	r1, [fp, #-16]
	bl	__umodsi3
	mov	r3, r0
	cmp	r3, #0
	movne	r3, #0
	moveq	r3, #1
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	divides, .-divides
	.align	2
	.global	even
	.type	even, %function
even:
	@ args = 0, pretend = 0, frame = 4
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #4
	str	r0, [fp, #-16]
	mov	r0, #2
	ldr	r1, [fp, #-16]
	bl	divides
	mov	r3, r0
	mov	r0, r3
	ldmfd	sp, {r3, fp, sp, pc}
	.size	even, .-even
	.align	2
	.global	prime
	.type	prime, %function
prime:
	@ args = 0, pretend = 0, frame = 12
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #12
	str	r0, [fp, #-20]
	ldr	r0, [fp, #-20]
	bl	even
	mov	r3, r0
	cmp	r3, #0
	beq	.L6
	ldr	r3, [fp, #-20]
	cmp	r3, #2
	movne	r3, #0
	moveq	r3, #1
	str	r3, [fp, #-24]
	b	.L8
.L6:
	mov	r3, #3
	str	r3, [fp, #-16]
	b	.L9
.L10:
	ldr	r0, [fp, #-16]
	ldr	r1, [fp, #-20]
	bl	divides
	mov	r3, r0
	cmp	r3, #0
	beq	.L11
	mov	r3, #0
	str	r3, [fp, #-24]
	b	.L8
.L11:
	ldr	r3, [fp, #-16]
	add	r3, r3, #2
	str	r3, [fp, #-16]
.L9:
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, [fp, #-20]
	cmp	r2, r3
	bls	.L10
	ldr	r3, [fp, #-20]
	cmp	r3, #1
	movls	r3, #0
	movhi	r3, #1
	str	r3, [fp, #-24]
.L8:
	ldr	r3, [fp, #-24]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	prime, .-prime
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
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 12
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #12
	ldr	r3, .L23
	str	r3, [fp, #-16]
	ldr	r3, .L23+4
	str	r3, [fp, #-20]
	sub	r3, fp, #16
	sub	r2, fp, #20
	mov	r0, r3
	mov	r1, r2
	bl	swap
	ldr	r3, [fp, #-16]
	mov	r0, r3
	bl	prime
	mov	r3, r0
	cmp	r3, #0
	beq	.L18
	ldr	r3, [fp, #-20]
	mov	r0, r3
	bl	prime
	mov	r3, r0
	cmp	r3, #0
	bne	.L20
.L18:
	mov	r3, #1
	str	r3, [fp, #-24]
	b	.L21
.L20:
	mov	r3, #0
	str	r3, [fp, #-24]
.L21:
	ldr	r3, [fp, #-24]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L24:
	.align	2
.L23:
	.word	21649
	.word	513239
	.size	main, .-main
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
