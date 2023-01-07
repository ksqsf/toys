	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	__Z13rcu_read_lockv             ## -- Begin function _Z13rcu_read_lockv
	.p2align	4, 0x90
__Z13rcu_read_lockv:                    ## @_Z13rcu_read_lockv
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	__Z15rcu_read_unlockv           ## -- Begin function _Z15rcu_read_unlockv
	.p2align	4, 0x90
__Z15rcu_read_unlockv:                  ## @_Z15rcu_read_unlockv
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	__Z8rcu_syncv                   ## -- Begin function _Z8rcu_syncv
	.p2align	4, 0x90
__Z8rcu_syncv:                          ## @_Z8rcu_syncv
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$4, %edi
	callq	__Znwm
	movl	$0, -4(%rbp)
	movl	(%rax), %ecx
	##MEMBARRIER
	movq	%rax, %rdi
	callq	__ZdlPv
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	__ZTW3tid                       ## -- Begin function _ZTW3tid
	.p2align	4, 0x90
__ZTW3tid:                              ## @_ZTW3tid
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	_tid@TLVP(%rip), %rdi
	callq	*(%rdi)
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
.tbss _tid$tlv$init, 4, 2               ## @tid

	.section	__DATA,__thread_vars,thread_local_variables
_tid:
	.quad	__tlv_bootstrap
	.quad	0
	.quad	_tid$tlv$init

	.globl	_global_epoch                   ## @global_epoch
.zerofill __DATA,__common,_global_epoch,4,2
	.globl	_epoch                          ## @epoch
.zerofill __DATA,__common,_epoch,16,4
.subsections_via_symbols
