	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_nothing                ## -- Begin function nothing
	.p2align	4, 0x90
_nothing:                               ## @nothing
	.cfi_startproc
## %bb.0:
	movl	$0, -4(%rsp)
	movl	$0, -8(%rsp)
	cmpl	$9, -8(%rsp)
	jg	LBB0_3
	.p2align	4, 0x90
LBB0_2:                                 ## %nothing.0_body
                                        ## =>This Inner Loop Header: Depth=1
	movl	-8(%rsp), %eax
	movl	%eax, -4(%rsp)
	incl	-8(%rsp)
	cmpl	$9, -8(%rsp)
	jle	LBB0_2
LBB0_3:                                 ## %nothing.0_end
	movl	-4(%rsp), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_a                      ## @a
	.p2align	2
_a:
	.long	3                       ## 0x3

	.globl	_b                      ## @b
	.p2align	2
_b:
	.long	1                       ## 0x1


.subsections_via_symbols
