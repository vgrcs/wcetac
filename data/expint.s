	.file	"expint.c"
	.text
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	mov	r0, #3
	mov	r1, #1
	bl	expint
	mov	r3, #0
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
	.size	main, .-main
	.align	2
	.global	foo
	.type	foo, %function
foo:
	@ args = 0, pretend = 0, frame = 4
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #4
	str	r0, [fp, #-16]
	ldr	r3, [fp, #-16]
	add	r2, r3, #8
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, [fp, #-16]
	rsb	r3, r3, #4
	mov	r3, r2, asl r3
	mov	r0, r3
	ldmfd	sp, {r3, fp, sp, pc}
	.size	foo, .-foo
	.global	__divsi3
	.align	2
	.global	expint
	.type	expint, %function
expint:
	@ args = 0, pretend = 0, frame = 64
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #64
	str	r0, [fp, #-64]
	str	r1, [fp, #-68]
	ldr	r3, [fp, #-64]
	sub	r3, r3, #1
	str	r3, [fp, #-52]
	ldr	r3, [fp, #-68]
	cmp	r3, #1
	ble	.L6
	ldr	r2, [fp, #-68]
	ldr	r3, [fp, #-64]
	add	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r3, .L27
	str	r3, [fp, #-40]
	ldr	r3, .L27+4
	str	r3, [fp, #-36]
	ldr	r3, [fp, #-36]
	str	r3, [fp, #-24]
	mov	r3, #1
	str	r3, [fp, #-60]
	b	.L8
.L9:
	ldr	r3, [fp, #-60]
	rsb	r1, r3, #0
	ldr	r2, [fp, #-52]
	ldr	r3, [fp, #-60]
	add	r3, r2, r3
	mul	r3, r1, r3
	str	r3, [fp, #-48]
	ldr	r3, [fp, #-44]
	add	r3, r3, #2
	str	r3, [fp, #-44]
	ldr	r2, [fp, #-48]
	ldr	r3, [fp, #-36]
	mul	r2, r3, r2
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	add	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r0, [fp, #-48]
	ldr	r1, [fp, #-40]
	bl	__divsi3
	mov	r3, r0
	mov	r2, r3
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	str	r3, [fp, #-40]
	ldr	r2, [fp, #-40]
	ldr	r3, [fp, #-36]
	mul	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-32]
	mul	r3, r2, r3
	str	r3, [fp, #-24]
	ldr	r2, [fp, #-32]
	ldr	r3, .L27+8
	cmp	r2, r3
	bgt	.L10
	ldr	r3, [fp, #-68]
	rsb	r2, r3, #0
	ldr	r3, [fp, #-24]
	mul	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	str	r3, [fp, #-76]
	b	.L12
.L10:
	ldr	r3, [fp, #-60]
	add	r3, r3, #1
	str	r3, [fp, #-60]
.L8:
	ldr	r3, [fp, #-60]
	cmp	r3, #2
	ble	.L9
	b	.L14
.L6:
	ldr	r3, [fp, #-52]
	cmp	r3, #0
	beq	.L15
	mov	r3, #2
	str	r3, [fp, #-72]
	b	.L17
.L15:
	mov	r3, #1000
	str	r3, [fp, #-72]
.L17:
	ldr	r3, [fp, #-72]
	str	r3, [fp, #-16]
	mov	r3, #1
	str	r3, [fp, #-28]
	mov	r3, #1
	str	r3, [fp, #-60]
	b	.L18
.L19:
	ldr	r3, [fp, #-68]
	rsb	r3, r3, #0
	mov	r0, r3
	ldr	r1, [fp, #-60]
	bl	__divsi3
	mov	r3, r0
	mov	r2, r3
	ldr	r3, [fp, #-28]
	mul	r3, r2, r3
	str	r3, [fp, #-28]
	ldr	r2, [fp, #-60]
	ldr	r3, [fp, #-52]
	cmp	r2, r3
	beq	.L20
	ldr	r3, [fp, #-28]
	rsb	r1, r3, #0
	ldr	r2, [fp, #-60]
	ldr	r3, [fp, #-52]
	rsb	r3, r3, r2
	mov	r0, r1
	mov	r1, r3
	bl	__divsi3
	mov	r3, r0
	str	r3, [fp, #-32]
	b	.L22
.L20:
	mov	r3, #255
	str	r3, [fp, #-20]
	mov	r3, #1
	str	r3, [fp, #-56]
	b	.L23
.L24:
	ldr	r2, [fp, #-56]
	ldr	r3, [fp, #-52]
	add	r2, r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r3, r2
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-56]
	add	r3, r3, #1
	str	r3, [fp, #-56]
.L23:
	ldr	r2, [fp, #-56]
	ldr	r3, [fp, #-52]
	cmp	r2, r3
	ble	.L24
	ldr	r0, [fp, #-68]
	bl	foo
	mov	r2, r0
	ldr	r3, [fp, #-28]
	mul	r2, r3, r2
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	str	r3, [fp, #-32]
.L22:
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-32]
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-60]
	add	r3, r3, #1
	str	r3, [fp, #-60]
.L18:
	ldr	r3, [fp, #-60]
	cmp	r3, #2
	ble	.L19
.L14:
	ldr	r3, [fp, #-16]
	str	r3, [fp, #-76]
.L12:
	ldr	r3, [fp, #-76]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L28:
	.align	2
.L27:
	.word	2000000
	.word	30000000
	.word	9999
	.size	expint, .-expint
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
