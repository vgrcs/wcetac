	.file	"crc.c"
	.global	lin
	.data
	.align	2
	.type	lin, %object
	.size	lin, 256
lin:
	.ascii	"asdffeagewaHAFEFaeDsFEawFdsFaefaeerdjgp\000"
	.space	216
	.text
	.align	2
	.global	icrc1
	.type	icrc1, %function
icrc1:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #16
	mov	r3, r0
	mov	r2, r1
	strh	r3, [fp, #-24]	@ movhi
	mov	r3, r2
	strb	r3, [fp, #-28]
	ldrb	r3, [fp, #-28]	@ zero_extendqisi2
	mov	r3, r3, asl #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	ldrh	r2, [fp, #-24]
	mov	r1, r3	@ movhi
	mov	r3, r2	@ movhi
	eor	r3, r1, r3
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [fp, #-14]	@ movhi
	mov	r3, #0
	str	r3, [fp, #-20]
	b	.L2
.L3:
	ldrh	r3, [fp, #-14]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	cmp	r3, #0
	bge	.L4
	ldrh	r3, [fp, #-14]	@ movhi
	mov	r3, r3, asl #1
	strh	r3, [fp, #-14]	@ movhi
	ldrh	r2, [fp, #-14]	@ movhi
	ldr	r3, .L9
	eor	r3, r2, r3
	strh	r3, [fp, #-14]	@ movhi
	b	.L6
.L4:
	ldrh	r3, [fp, #-14]	@ movhi
	mov	r3, r3, asl #1
	strh	r3, [fp, #-14]	@ movhi
.L6:
	ldr	r3, [fp, #-20]
	add	r3, r3, #1
	str	r3, [fp, #-20]
.L2:
	ldr	r3, [fp, #-20]
	cmp	r3, #7
	ble	.L3
	ldrh	r3, [fp, #-14]
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L10:
	.align	2
.L9:
	.word	4129
	.size	icrc1, .-icrc1
	.data
	.type	it.1279, %object
	.size	it.1279, 16
it.1279:
	.byte	0
	.byte	8
	.byte	4
	.byte	12
	.byte	2
	.byte	10
	.byte	6
	.byte	14
	.byte	1
	.byte	9
	.byte	5
	.byte	13
	.byte	3
	.byte	11
	.byte	7
	.byte	15
	.local	rchr.1274
	.comm	rchr.1274,256,1
	.local	init.1273
	.comm	init.1273,2,2
	.local	icrctb.1272
	.comm	icrctb.1272,512,2
	.text
	.align	2
	.global	icrc
	.type	icrc, %function
icrc:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {r4, fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #24
	str	r1, [fp, #-32]
	str	r3, [fp, #-40]
	strh	r0, [fp, #-28]	@ movhi
	strh	r2, [fp, #-36]	@ movhi
	ldrh	r3, [fp, #-28]	@ movhi
	strh	r3, [fp, #-18]	@ movhi
	ldr	r3, .L30
	ldrh	r3, [r3, #0]
	cmp	r3, #0
	bne	.L12
	ldr	r2, .L30
	mov	r3, #1
	strh	r3, [r2, #0]	@ movhi
	mov	r3, #0
	strh	r3, [fp, #-20]	@ movhi
	b	.L14
.L15:
	ldrh	r4, [fp, #-20]
	ldrh	r3, [fp, #-20]	@ movhi
	mov	r3, r3, asl #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	mov	r0, r3
	mov	r1, #0
	bl	icrc1
	mov	r3, r0
	mov	r1, r3
	ldr	r2, .L30+4
	mov	r3, r4, asl #1
	add	r3, r3, r2
	strh	r1, [r3, #0]	@ movhi
	ldrh	r0, [fp, #-20]
	ldrh	r3, [fp, #-20]
	and	r2, r3, #15
	ldr	r3, .L30+8
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r3, r3, asl #4
	and	r1, r3, #255
	ldrh	r3, [fp, #-20]
	mov	r3, r3, lsr #4
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	mov	r2, r3
	ldr	r3, .L30+8
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r2, r1
	orr	r3, r2, r3
	and	r3, r3, #255
	and	r3, r3, #255
	ldr	r2, .L30+12
	strb	r3, [r2, r0]
	ldrh	r3, [fp, #-20]	@ movhi
	add	r3, r3, #1
	strh	r3, [fp, #-20]	@ movhi
.L14:
	ldrh	r3, [fp, #-20]
	cmp	r3, #2
	bls	.L15
.L12:
	ldrsh	r3, [fp, #-36]
	cmp	r3, #0
	blt	.L16
	ldrh	r3, [fp, #-36]	@ movhi
	and	r3, r3, #255
	mov	r2, r3
	ldrh	r3, [fp, #-36]	@ movhi
	and	r3, r3, #255
	mov	r3, r3, asl #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	orr	r3, r2, r3
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [fp, #-18]	@ movhi
	b	.L18
.L16:
	ldr	r3, [fp, #-40]
	cmp	r3, #0
	bge	.L18
	ldrh	r3, [fp, #-18]
	mov	r3, r3, lsr #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	and	r3, r3, #255
	mov	r2, r3
	ldr	r3, .L30+12
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r1, r3
	ldrh	r3, [fp, #-18]	@ movhi
	and	r3, r3, #255
	and	r3, r3, #255
	mov	r2, r3
	ldr	r3, .L30+12
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r3, r3, asl #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	mov	r2, r1	@ movhi
	orr	r3, r2, r3
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [fp, #-18]	@ movhi
.L18:
	mov	r3, #1
	strh	r3, [fp, #-20]	@ movhi
	b	.L20
.L21:
	ldr	r3, [fp, #-40]
	cmp	r3, #0
	bge	.L22
	ldrh	r2, [fp, #-20]
	ldr	r3, .L30+16
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r2, r3
	ldr	r3, .L30+12
	ldrb	r2, [r3, r2]	@ zero_extendqisi2
	ldrh	r3, [fp, #-18]
	mov	r3, r3, lsr #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	and	r3, r3, #255
	eor	r3, r2, r3
	and	r3, r3, #255
	strh	r3, [fp, #-24]	@ movhi
	b	.L24
.L22:
	ldrh	r2, [fp, #-20]
	ldr	r3, .L30+16
	ldrb	r2, [r3, r2]	@ zero_extendqisi2
	ldrh	r3, [fp, #-18]
	mov	r3, r3, lsr #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	and	r3, r3, #255
	eor	r3, r2, r3
	and	r3, r3, #255
	strh	r3, [fp, #-24]	@ movhi
.L24:
	ldrh	r3, [fp, #-24]
	ldr	r2, .L30+4
	mov	r3, r3, asl #1
	add	r3, r3, r2
	ldrh	r3, [r3, #0]
	mov	r2, r3
	ldrh	r3, [fp, #-18]	@ movhi
	and	r3, r3, #255
	and	r3, r3, #255
	mov	r3, r3, asl #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	eor	r3, r2, r3
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [fp, #-18]	@ movhi
	ldrh	r3, [fp, #-20]	@ movhi
	add	r3, r3, #1
	strh	r3, [fp, #-20]	@ movhi
.L20:
	ldrh	r2, [fp, #-20]
	ldr	r3, [fp, #-32]
	cmp	r2, r3
	bls	.L21
	ldr	r3, [fp, #-40]
	cmp	r3, #0
	blt	.L26
	ldrh	r3, [fp, #-18]	@ movhi
	strh	r3, [fp, #-22]	@ movhi
	b	.L28
.L26:
	ldrh	r3, [fp, #-18]
	mov	r3, r3, lsr #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	and	r3, r3, #255
	mov	r2, r3
	ldr	r3, .L30+12
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r1, r3
	ldrh	r3, [fp, #-18]	@ movhi
	and	r3, r3, #255
	and	r3, r3, #255
	mov	r2, r3
	ldr	r3, .L30+12
	ldrb	r3, [r3, r2]	@ zero_extendqisi2
	mov	r3, r3, asl #8
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	mov	r2, r1	@ movhi
	orr	r3, r2, r3
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [fp, #-22]	@ movhi
.L28:
	ldrh	r3, [fp, #-22]
	mov	r0, r3
	sub	sp, fp, #16
	ldmfd	sp, {r4, fp, sp, pc}
.L31:
	.align	2
.L30:
	.word	init.1273
	.word	icrctb.1272
	.word	it.1279
	.word	rchr.1274
	.word	lin
	.size	icrc, .-icrc
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #8
	mov	r3, #3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	add	r1, r3, #1
	ldr	r2, .L34
	mov	r3, #0
	strb	r3, [r2, r1]
	mov	r0, #0
	ldr	r1, [fp, #-16]
	mov	r2, #0
	mov	r3, #1
	bl	icrc
	mov	r3, r0
	strh	r3, [fp, #-20]	@ movhi
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L35:
	.align	2
.L34:
	.word	lin
	.size	main, .-main
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
