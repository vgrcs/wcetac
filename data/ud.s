	.file	"ud.c"
	.global	__floatsidf
	.global	__adddf3
	.global	__fixdfsi
	.text
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {r4, r5, r6, fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #24
	mov	r3, #0
	str	r3, [fp, #-44]
	mov	r3, #5
	str	r3, [fp, #-40]
	mov	r3, #5
	str	r3, [fp, #-36]
	mov	r3, #0
	str	r3, [fp, #-44]
	b	.L2
.L3:
	ldr	r1, [fp, #-48]
	ldr	r0, [fp, #-44]
	ldr	r2, [fp, #-48]
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	add	ip, r3, #2
	ldr	r2, .L8
	mov	r3, r1
	mov	r3, r3, asl #2
	add	r3, r3, r1
	add	r3, r3, r0
	str	ip, [r2, r3, asl #2]
	ldr	r2, [fp, #-48]
	ldr	r3, [fp, #-44]
	cmp	r2, r3
	bne	.L4
	ldr	r5, [fp, #-48]
	ldr	r6, [fp, #-44]
	ldr	r2, [fp, #-48]
	ldr	r1, [fp, #-44]
	ldr	r0, .L8
	mov	r3, r2
	mov	r3, r3, asl #2
	add	r3, r3, r2
	add	r3, r3, r1
	ldr	r3, [r0, r3, asl #2]
	mov	r0, r3
	bl	__floatsidf
	mov	r3, r0
	mov	r4, r1
	mov	r0, r3
	mov	r1, r4
	mov	r2, r3
	mov	r3, r4
	bl	__adddf3
	mov	r3, r0
	mov	r4, r1
	mov	r0, r3
	mov	r1, r4
	bl	__fixdfsi
	mov	r1, r0
	ldr	r2, .L8
	mov	r3, r5
	mov	r3, r3, asl #2
	add	r3, r3, r5
	add	r3, r3, r6
	str	r1, [r2, r3, asl #2]
.L4:
	ldr	r2, [fp, #-48]
	ldr	r1, [fp, #-44]
	ldr	r0, .L8
	mov	r3, r2
	mov	r3, r3, asl #2
	add	r3, r3, r2
	add	r3, r3, r1
	ldr	r2, [r0, r3, asl #2]
	ldr	r3, [fp, #-28]
	add	r3, r3, r2
	str	r3, [fp, #-28]
	ldr	r3, [fp, #-44]
	add	r3, r3, #1
	str	r3, [fp, #-44]
.L2:
	ldr	r2, [fp, #-44]
	ldr	r3, [fp, #-36]
	cmp	r2, r3
	ble	.L3
	ldr	r1, [fp, #-48]
	ldr	r2, .L8+4
	ldr	r3, [fp, #-28]
	str	r3, [r2, r1, asl #2]
	sub	sp, fp, #24
	ldmfd	sp, {r4, r5, r6, fp, sp, pc}
.L9:
	.align	2
.L8:
	.word	a
	.word	b
	.size	main, .-main
	.comm	a,100,4
	.comm	b,20,4
	.comm	x,20,4
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
