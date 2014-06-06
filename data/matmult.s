	.file	"matmult.c"
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
	bl	InitSeed
	ldr	r0, .L3
	ldr	r1, .L3+4
	ldr	r2, .L3+8
	bl	Test
	mov	r3, #0
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
.L4:
	.align	2
.L3:
	.word	ArrayA
	.word	ArrayB
	.word	ResultArray
	.size	main, .-main
	.align	2
	.global	InitSeed
	.type	InitSeed, %function
InitSeed:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	ldr	r2, .L7
	mov	r3, #0
	str	r3, [r2, #0]
	ldmfd	sp, {fp, sp, pc}
.L8:
	.align	2
.L7:
	.word	Seed
	.size	InitSeed, .-InitSeed
	.align	2
	.global	Test
	.type	Test, %function
Test:
	@ args = 0, pretend = 0, frame = 12
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #12
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	ldr	r0, [fp, #-16]
	bl	Initialize
	ldr	r0, [fp, #-20]
	bl	Initialize
	ldr	r0, [fp, #-16]
	ldr	r1, [fp, #-20]
	ldr	r2, [fp, #-24]
	bl	Multiply
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	Test, .-Test
	.align	2
	.global	Initialize
	.type	Initialize, %function
Initialize:
	@ args = 0, pretend = 0, frame = 12
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #12
	str	r0, [fp, #-24]
	mov	r3, #0
	str	r3, [fp, #-20]
	b	.L12
.L13:
	mov	r3, #0
	str	r3, [fp, #-16]
	b	.L14
.L15:
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-24]
	add	r2, r2, r3
	ldr	r3, [fp, #-16]
	mov	r3, r3, asl #2
	add	r2, r3, r2
	mov	r3, #0
	str	r3, [r2, #0]
	ldr	r3, [fp, #-16]
	add	r3, r3, #1
	str	r3, [fp, #-16]
.L14:
	ldr	r3, [fp, #-16]
	cmp	r3, #1
	ble	.L15
	ldr	r3, [fp, #-20]
	add	r3, r3, #1
	str	r3, [fp, #-20]
.L12:
	ldr	r3, [fp, #-20]
	cmp	r3, #1
	ble	.L13
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	Initialize, .-Initialize
	.align	2
	.global	RandomInteger
	.type	RandomInteger, %function
RandomInteger:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	ldr	r2, .L21
	mov	r3, #15
	str	r3, [r2, #0]
	ldr	r3, .L21
	ldr	r3, [r3, #0]
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
.L22:
	.align	2
.L21:
	.word	Seed
	.size	RandomInteger, .-RandomInteger
	.align	2
	.global	Multiply
	.type	Multiply, %function
Multiply:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #24
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	mov	r3, #0
	str	r3, [fp, #-36]
	b	.L24
.L25:
	mov	r3, #0
	str	r3, [fp, #-32]
	b	.L26
.L27:
	ldr	r3, [fp, #-36]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-24]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	mov	r3, r3, asl #2
	add	r2, r3, r2
	mov	r3, #0
	str	r3, [r2, #0]
	mov	r3, #0
	str	r3, [fp, #-28]
	b	.L28
.L29:
	ldr	r3, [fp, #-36]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-24]
	add	r0, r2, r3
	ldr	ip, [fp, #-32]
	ldr	r3, [fp, #-36]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-24]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	mov	r3, r3, asl #2
	add	r3, r3, r2
	ldr	lr, [r3, #0]
	ldr	r3, [fp, #-36]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-16]
	add	r2, r2, r3
	ldr	r3, [fp, #-28]
	mov	r3, r3, asl #2
	add	r3, r3, r2
	ldr	r1, [r3, #0]
	ldr	r3, [fp, #-28]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	mov	r3, r3, asl #2
	add	r3, r3, r2
	ldr	r3, [r3, #0]
	mul	r3, r1, r3
	add	r2, lr, r3
	mov	r3, ip, asl #2
	add	r3, r3, r0
	str	r2, [r3, #0]
	ldr	r3, [fp, #-28]
	add	r3, r3, #1
	str	r3, [fp, #-28]
.L28:
	ldr	r3, [fp, #-28]
	cmp	r3, #1
	ble	.L29
	ldr	r3, [fp, #-32]
	add	r3, r3, #1
	str	r3, [fp, #-32]
.L26:
	ldr	r3, [fp, #-32]
	cmp	r3, #1
	ble	.L27
	ldr	r3, [fp, #-36]
	add	r3, r3, #1
	str	r3, [fp, #-36]
.L24:
	ldr	r3, [fp, #-36]
	cmp	r3, #1
	ble	.L25
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	Multiply, .-Multiply
	.comm	Seed,4,4
	.comm	ArrayA,16,4
	.comm	ArrayB,16,4
	.comm	ResultArray,16,4
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
