	.file	"bs3.c"
	.global	data
	.data
	.align	2
	.type	data, %object
	.size	data, 24
data:
	.word	1
	.word	100
	.word	5
	.word	200
	.word	6
	.word	300
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
	mov	r0, #1
	bl	binary_search
	mov	r3, #0
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
	.size	main, .-main
	.align	2
	.global	binary_search
	.type	binary_search, %function
binary_search:
	@ args = 0, pretend = 0, frame = 20
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #20
	str	r0, [fp, #-32]
	mov	r3, #0
	str	r3, [fp, #-16]
	mov	r3, #2
	str	r3, [fp, #-20]
	mvn	r3, #0
	str	r3, [fp, #-28]
	b	.L4
.L5:
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	mov	r3, r3, asr #1
	str	r3, [fp, #-24]
	ldr	r2, [fp, #-24]
	ldr	r3, .L13
	ldr	r2, [r3, r2, asl #3]
	ldr	r3, [fp, #-32]
	cmp	r2, r3
	bne	.L6
	ldr	r3, [fp, #-16]
	sub	r3, r3, #1
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-24]
	ldr	r2, .L13
	mov	r1, #4
	mov	r3, r3, asl #3
	add	r3, r3, r2
	add	r3, r3, r1
	ldr	r3, [r3, #0]
	str	r3, [fp, #-28]
	b	.L8
.L6:
	ldr	r2, [fp, #-24]
	ldr	r3, .L13
	ldr	r2, [r3, r2, asl #3]
	ldr	r3, [fp, #-32]
	cmp	r2, r3
	ble	.L9
	ldr	r3, [fp, #-24]
	sub	r3, r3, #1
	str	r3, [fp, #-20]
	b	.L8
.L9:
	ldr	r3, [fp, #-24]
	add	r3, r3, #1
	str	r3, [fp, #-16]
.L8:
	ldr	r3, .L13+4
	ldr	r3, [r3, #0]
	add	r2, r3, #1
	ldr	r3, .L13+4
	str	r2, [r3, #0]
.L4:
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	cmp	r2, r3
	ble	.L5
	ldr	r3, [fp, #-28]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L14:
	.align	2
.L13:
	.word	data
	.word	cnt1
	.size	binary_search, .-binary_search
	.comm	cnt1,4,4
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
