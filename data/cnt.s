	.file	"cnt.c"
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
	bl	Test
	mov	r3, #1
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
.L4:
	.align	2
.L3:
	.word	Array
	.size	main, .-main
	.align	2
	.global	Test
	.type	Test, %function
Test:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #16
	str	r0, [fp, #-28]
	ldr	r0, [fp, #-28]
	bl	Initialize
	ldr	r0, [fp, #-28]
	bl	Sum
	mov	r3, #0
	mov	r0, r3
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
	str	r0, [fp, #-16]
	mov	r3, #0
	str	r3, [fp, #-24]
	b	.L8
.L9:
	mov	r3, #0
	str	r3, [fp, #-20]
	b	.L10
.L11:
	ldr	r3, [fp, #-24]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-16]
	add	r1, r2, r3
	ldr	r2, [fp, #-20]
	mov	r3, #0
	str	r3, [r1, r2, asl #2]
	ldr	r3, [fp, #-20]
	add	r3, r3, #1
	str	r3, [fp, #-20]
.L10:
	ldr	r3, [fp, #-20]
	cmp	r3, #0
	ble	.L11
	ldr	r3, [fp, #-24]
	add	r3, r3, #1
	str	r3, [fp, #-24]
.L8:
	ldr	r3, [fp, #-24]
	cmp	r3, #0
	ble	.L9
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	Initialize, .-Initialize
	.align	2
	.global	InitSeed
	.type	InitSeed, %function
InitSeed:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	ldr	r2, .L17
	mov	r3, #0
	str	r3, [r2, #0]
	mov	r3, #0
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
.L18:
	.align	2
.L17:
	.word	Seed
	.size	InitSeed, .-InitSeed
	.align	2
	.global	Sum
	.type	Sum, %function
Sum:
	@ args = 0, pretend = 0, frame = 28
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #28
	str	r0, [fp, #-32]
	mov	r3, #0
	str	r3, [fp, #-28]
	mov	r3, #0
	str	r3, [fp, #-24]
	mov	r3, #0
	str	r3, [fp, #-20]
	mov	r3, #0
	str	r3, [fp, #-16]
	mov	r3, #0
	str	r3, [fp, #-40]
	b	.L20
.L21:
	mov	r3, #0
	str	r3, [fp, #-36]
	b	.L22
.L23:
	ldr	r3, [fp, #-40]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-32]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	ldr	r3, [r2, r3, asl #2]
	cmp	r3, #0
	bge	.L24
	ldr	r3, [fp, #-40]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-32]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	ldr	r2, [r2, r3, asl #2]
	ldr	r3, [fp, #-28]
	add	r3, r3, r2
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-20]
	add	r3, r3, #1
	str	r3, [fp, #-20]
	b	.L26
.L24:
	ldr	r3, [fp, #-40]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-32]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	ldr	r2, [r2, r3, asl #2]
	ldr	r3, [fp, #-24]
	add	r3, r3, r2
	str	r3, [fp, #-24]
	ldr	r3, [fp, #-16]
	add	r3, r3, #1
	str	r3, [fp, #-16]
.L26:
	ldr	r3, [fp, #-36]
	add	r3, r3, #1
	str	r3, [fp, #-36]
.L22:
	ldr	r3, [fp, #-36]
	cmp	r3, #0
	ble	.L23
	ldr	r3, [fp, #-40]
	add	r3, r3, #1
	str	r3, [fp, #-40]
.L20:
	ldr	r3, [fp, #-40]
	cmp	r3, #0
	ble	.L21
	ldr	r2, .L30
	ldr	r3, [fp, #-28]
	str	r3, [r2, #0]
	ldr	r2, .L30+4
	ldr	r3, [fp, #-20]
	str	r3, [r2, #0]
	ldr	r2, .L30+8
	ldr	r3, [fp, #-24]
	str	r3, [r2, #0]
	ldr	r2, .L30+12
	ldr	r3, [fp, #-16]
	str	r3, [r2, #0]
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L31:
	.align	2
.L30:
	.word	Postotal
	.word	Poscnt
	.word	Negtotal
	.word	Negcnt
	.size	Sum, .-Sum
	.comm	Seed,4,4
	.comm	Array,4,4
	.comm	Postotal,4,4
	.comm	Negtotal,4,4
	.comm	Poscnt,4,4
	.comm	Negcnt,4,4
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
