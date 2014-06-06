	.file	"fdct.c"
	.global	block
	.data
	.align	1
	.type	block, %object
	.size	block, 128
block:
	.short	99
	.short	104
	.short	109
	.short	113
	.short	115
	.short	115
	.short	55
	.short	55
	.short	104
	.short	111
	.short	113
	.short	116
	.short	119
	.short	56
	.short	56
	.short	56
	.short	110
	.short	115
	.short	120
	.short	119
	.short	118
	.short	56
	.short	56
	.short	56
	.short	119
	.short	121
	.short	122
	.short	120
	.short	120
	.short	59
	.short	59
	.short	59
	.short	119
	.short	120
	.short	121
	.short	122
	.short	122
	.short	55
	.short	55
	.short	55
	.short	121
	.short	121
	.short	121
	.short	121
	.short	60
	.short	57
	.short	57
	.short	57
	.short	122
	.short	122
	.short	61
	.short	63
	.short	62
	.short	57
	.short	57
	.short	57
	.short	62
	.short	62
	.short	61
	.short	61
	.short	63
	.short	58
	.short	58
	.short	58
	.text
	.align	2
	.global	fdct
	.type	fdct, %function
fdct:
	@ args = 0, pretend = 0, frame = 88
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	sub	sp, sp, #88
	str	r0, [fp, #-96]
	str	r1, [fp, #-100]
	ldr	r3, [fp, #-96]
	str	r3, [fp, #-20]
	mov	r3, #0
	str	r3, [fp, #-24]
	b	.L2
.L3:
	ldr	r3, [fp, #-20]
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #14
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r2, r3
	str	r3, [fp, #-92]
	ldr	r3, [fp, #-20]
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #14
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r2
	str	r3, [fp, #-64]
	ldr	r3, [fp, #-20]
	add	r3, r3, #2
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #12
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r2, r3
	str	r3, [fp, #-88]
	ldr	r3, [fp, #-20]
	add	r3, r3, #2
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #12
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r2
	str	r3, [fp, #-68]
	ldr	r3, [fp, #-20]
	add	r3, r3, #4
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #10
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r2, r3
	str	r3, [fp, #-84]
	ldr	r3, [fp, #-20]
	add	r3, r3, #4
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #10
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r2
	str	r3, [fp, #-72]
	ldr	r3, [fp, #-20]
	add	r3, r3, #6
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #8
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r2, r3
	str	r3, [fp, #-80]
	ldr	r3, [fp, #-20]
	add	r3, r3, #6
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r2, r3, asr #16
	ldr	r3, [fp, #-20]
	add	r3, r3, #8
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r2
	str	r3, [fp, #-76]
	ldr	r2, [fp, #-92]
	ldr	r3, [fp, #-80]
	add	r3, r2, r3
	str	r3, [fp, #-60]
	ldr	r2, [fp, #-92]
	ldr	r3, [fp, #-80]
	rsb	r3, r3, r2
	str	r3, [fp, #-48]
	ldr	r2, [fp, #-88]
	ldr	r3, [fp, #-84]
	add	r3, r2, r3
	str	r3, [fp, #-56]
	ldr	r2, [fp, #-88]
	ldr	r3, [fp, #-84]
	rsb	r3, r3, r2
	str	r3, [fp, #-52]
	ldr	r2, [fp, #-60]
	ldr	r3, [fp, #-56]
	add	r3, r2, r3
	mov	r3, r3, asl #2
	mov	r3, r3, asl #16
	mov	r2, r3, lsr #16
	ldr	r3, [fp, #-20]
	strh	r2, [r3, #0]	@ movhi
	ldr	r3, [fp, #-20]
	add	r1, r3, #8
	ldr	r2, [fp, #-60]
	ldr	r3, [fp, #-56]
	rsb	r3, r3, r2
	mov	r3, r3, asl #2
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, .L9
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-52]
	ldr	r3, [fp, #-48]
	add	r2, r2, r3
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r3, .L9+4
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-20]
	add	r1, r3, #4
	ldr	r2, [fp, #-48]
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	mov	r3, r3, asr #11
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, .L9+8
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-20]
	add	r1, r3, #12
	ldr	r2, [fp, #-52]
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	mov	r3, r3, asr #11
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-64]
	add	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-68]
	add	r3, r2, r3
	str	r3, [fp, #-40]
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-68]
	add	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-64]
	add	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r3, .L9+12
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-36]
	ldr	r3, [fp, #-32]
	add	r2, r2, r3
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-28]
	ldr	r3, .L9+16
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-76]
	ldr	r3, .L9+20
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-72]
	ldr	r3, .L9+24
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-68]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-68]
	ldr	r3, .L9+28
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-64]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-64]
	ldr	r3, .L9+32
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-44]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r3, .L9+36
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-40]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-40]
	ldr	r3, .L9+40
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-36]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r3, .L9+44
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-32]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r2, [fp, #-36]
	ldr	r3, [fp, #-28]
	add	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r2, [fp, #-32]
	ldr	r3, [fp, #-28]
	add	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r3, [fp, #-20]
	add	r1, r3, #14
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-44]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	mov	r3, r3, asr #11
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-20]
	add	r1, r3, #10
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-40]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	add	r3, r2, r3
	mov	r3, r3, asr #11
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-20]
	add	r1, r3, #6
	ldr	r2, [fp, #-68]
	ldr	r3, [fp, #-40]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	mov	r3, r3, asr #11
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-20]
	add	r1, r3, #2
	ldr	r2, [fp, #-64]
	ldr	r3, [fp, #-44]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	add	r3, r2, r3
	mov	r3, r3, asr #11
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #1
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r3, r2
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-24]
	add	r3, r3, #1
	str	r3, [fp, #-24]
.L2:
	ldr	r3, [fp, #-24]
	cmp	r3, #1
	ble	.L3
	ldr	r3, [fp, #-96]
	str	r3, [fp, #-20]
	mov	r3, #0
	str	r3, [fp, #-24]
	b	.L5
.L6:
	ldr	r3, [fp, #-20]
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #3
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r1, r3
	str	r3, [fp, #-92]
	ldr	r3, [fp, #-20]
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #3
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r1
	str	r3, [fp, #-64]
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #1
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #2
	mov	r3, r2, asl #2
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r1, r3
	str	r3, [fp, #-88]
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #1
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #2
	mov	r3, r2, asl #2
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r1
	str	r3, [fp, #-68]
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	add	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r1, r3
	str	r3, [fp, #-84]
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	add	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r1
	str	r3, [fp, #-72]
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	add	r3, r1, r3
	str	r3, [fp, #-80]
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r1, r3, asr #16
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	rsb	r3, r3, r1
	str	r3, [fp, #-76]
	ldr	r2, [fp, #-92]
	ldr	r3, [fp, #-80]
	add	r3, r2, r3
	str	r3, [fp, #-60]
	ldr	r2, [fp, #-92]
	ldr	r3, [fp, #-80]
	rsb	r3, r3, r2
	str	r3, [fp, #-48]
	ldr	r2, [fp, #-88]
	ldr	r3, [fp, #-84]
	add	r3, r2, r3
	str	r3, [fp, #-56]
	ldr	r2, [fp, #-88]
	ldr	r3, [fp, #-84]
	rsb	r3, r3, r2
	str	r3, [fp, #-52]
	ldr	r2, [fp, #-60]
	ldr	r3, [fp, #-56]
	add	r3, r2, r3
	mov	r3, r3, asr #5
	mov	r3, r3, asl #16
	mov	r2, r3, lsr #16
	ldr	r3, [fp, #-20]
	strh	r2, [r3, #0]	@ movhi
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-60]
	ldr	r3, [fp, #-56]
	rsb	r3, r3, r2
	mov	r3, r3, asr #5
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, .L9
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-52]
	ldr	r3, [fp, #-48]
	add	r2, r2, r3
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r3, .L9+4
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #2
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-48]
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	mov	r3, r3, asr #18
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, .L9+8
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #2
	mov	r3, r2, asl #2
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-52]
	ldr	r3, [fp, #-16]
	mul	r2, r3, r2
	ldr	r3, [fp, #-44]
	add	r3, r2, r3
	mov	r3, r3, asr #18
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-64]
	add	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-68]
	add	r3, r2, r3
	str	r3, [fp, #-40]
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-68]
	add	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-64]
	add	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r3, .L9+12
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-36]
	ldr	r3, [fp, #-32]
	add	r2, r2, r3
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-28]
	ldr	r3, .L9+16
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-76]
	ldr	r3, .L9+20
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-72]
	ldr	r3, .L9+24
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-68]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-68]
	ldr	r3, .L9+28
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-64]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-64]
	ldr	r3, .L9+32
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-44]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-44]
	ldr	r3, .L9+36
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-40]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-40]
	ldr	r3, .L9+40
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-36]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r3, .L9+44
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-32]
	ldr	r3, [fp, #-16]
	mul	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r2, [fp, #-36]
	ldr	r3, [fp, #-28]
	add	r3, r2, r3
	str	r3, [fp, #-36]
	ldr	r2, [fp, #-32]
	ldr	r3, [fp, #-28]
	add	r3, r2, r3
	str	r3, [fp, #-32]
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #3
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-44]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	mov	r3, r3, asr #18
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	add	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-72]
	ldr	r3, [fp, #-40]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	add	r3, r2, r3
	mov	r3, r3, asr #18
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-100]
	mov	r2, r3, asl #1
	mov	r3, r2, asl #2
	rsb	r3, r2, r3
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-68]
	ldr	r3, [fp, #-40]
	add	r2, r2, r3
	ldr	r3, [fp, #-36]
	add	r3, r2, r3
	mov	r3, r3, asr #18
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-100]
	mov	r3, r3, asl #1
	mov	r2, r3
	ldr	r3, [fp, #-20]
	add	r1, r2, r3
	ldr	r2, [fp, #-64]
	ldr	r3, [fp, #-44]
	add	r2, r2, r3
	ldr	r3, [fp, #-32]
	add	r3, r2, r3
	mov	r3, r3, asr #18
	mov	r3, r3, asl #16
	mov	r3, r3, lsr #16
	strh	r3, [r1, #0]	@ movhi
	ldr	r3, [fp, #-20]
	add	r3, r3, #2
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-24]
	add	r3, r3, #1
	str	r3, [fp, #-24]
.L5:
	ldr	r3, [fp, #-24]
	cmp	r3, #1
	ble	.L6
	sub	sp, fp, #12
	ldmfd	sp, {fp, sp, pc}
.L10:
	.align	2
.L9:
	.word	4433
	.word	6270
	.word	-15137
	.word	9633
	.word	2446
	.word	16819
	.word	25172
	.word	12299
	.word	-7373
	.word	-20995
	.word	-16069
	.word	-3196
	.size	fdct, .-fdct
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	mov	ip, sp
	stmfd	sp!, {fp, ip, lr, pc}
	sub	fp, ip, #4
	ldr	r0, .L13
	mov	r1, #8
	bl	fdct
	ldr	r3, .L13
	ldrh	r3, [r3, #0]
	mov	r3, r3, asl #16
	mov	r3, r3, asr #16
	mov	r0, r3
	ldmfd	sp, {fp, sp, pc}
.L14:
	.align	2
.L13:
	.word	block
	.size	main, .-main
	.comm	out,4,4
	.ident	"GCC: (GNU) 4.1.2 (Fedora GP2X 4.1.2-13.fc17)"
