	.file	"bsort100.c"
	.text
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
	ldr	r0, .L3
	bl	Initialize
	ldr	r0, .L3
	bl	BubbleSort
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L4:
	.align	2
.L3:
	.word	Array
	.size	main, .-main
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
	ldr	r2, .L10
	mvn	r3, #0
	str	r3, [r2, #0]
	ldr	r3, .L10
	ldr	r3, [r3, #0]
	str	r3, [fp, #-16]
	mov	r3, #1
	str	r3, [fp, #-20]
	b	.L6
.L7:
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-24]
	add	r1, r2, r3
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, .L10+4
	ldrb	r3, [r3, #0]	@ zero_extendqisi2
	mul	r3, r2, r3
	str	r3, [r1, #0]
	ldr	r3, [fp, #-20]
	add	r3, r3, #1
	str	r3, [fp, #-20]
.L6:
	ldr	r3, [fp, #-20]
	cmp	r3, #2
	ble	.L7
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L11:
	.align	2
.L10:
	.word	factor
	.word	-2145386495
	.size	Initialize, .-Initialize
	.align	2
	.global	BubbleSort
	.type	BubbleSort, %function
BubbleSort:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #24
	str	r0, [fp, #-36]
	mov	r3, #0
	str	r3, [fp, #-32]
	mov	r3, #1
	str	r3, [fp, #-16]
	b	.L13
.L14:
	mov	r3, #1
	str	r3, [fp, #-32]
	mov	r3, #1
	str	r3, [fp, #-20]
	b	.L15
.L16:
	ldr	r3, [fp, #-16]
	rsb	r2, r3, #2
	ldr	r3, [fp, #-20]
	cmp	r2, r3
	blt	.L17
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	ldr	r1, [r3, #0]
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	add	r3, r3, #4
	ldr	r3, [r3, #0]
	cmp	r1, r3
	ble	.L19
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	ldr	r3, [r3, #0]
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-36]
	add	r1, r2, r3
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	add	r3, r3, #4
	ldr	r3, [r3, #0]
	str	r3, [r1, #0]
	ldr	r3, [fp, #-20]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	add	r2, r3, #4
	ldr	r3, [fp, #-28]
	str	r3, [r2, #0]
	mov	r3, #0
	str	r3, [fp, #-32]
.L19:
	ldr	r3, [fp, #-20]
	add	r3, r3, #1
	str	r3, [fp, #-20]
.L15:
	ldr	r3, [fp, #-20]
	cmp	r3, #1
	ble	.L16
.L17:
	ldr	r3, [fp, #-32]
	cmp	r3, #0
	bne	.L24
	ldr	r3, [fp, #-16]
	add	r3, r3, #1
	str	r3, [fp, #-16]
.L13:
	ldr	r3, [fp, #-16]
	cmp	r3, #1
	ble	.L14
.L24:
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
	.size	BubbleSort, .-BubbleSort
	.comm	Array,12,4
	.comm	Seed,4,4
	.comm	factor,4,4
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
