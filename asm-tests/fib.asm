BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
section .text
global _main
_fib:
push   rbp
mov    rbp, rsp

.ifcond_0:
sub    rsp, 16
mov    rax, [rbp+16]
mov    qword [rsp + 8], rax
mov    rax, 0
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_1
mov    rax, 0
jmp    .if_done1
.if_true_1:
mov    rax, 1
.if_done1:
add    rsp, 16

cmp    rax, 0
je     .if_done_0
mov    rax, 0
mov    rsp, rbp
pop    rbp
ret
.if_done_0:
.ifcond_2:
sub    rsp, 16
mov    rax, [rbp+16]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_3
mov    rax, 0
jmp    .if_done3
.if_true_3:
mov    rax, 1
.if_done3:
add    rsp, 16

cmp    rax, 0
je     .if_done_2
mov    rax, 1
mov    rsp, rbp
pop    rbp
ret
.if_done_2:
sub    rsp, 16
sub    rsp, 8
sub    rsp, 16
mov    rax, [rbp+16]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
push   rax
call   _fib
add    rsp, 16
mov    qword [rsp + 8], rax
sub    rsp, 8
sub    rsp, 16
mov    rax, [rbp+16]
mov    qword [rsp + 8], rax
mov    rax, 2
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
push   rax
call   _fib
add    rsp, 16
mov    qword [rsp], rax
mov    rax, [rsp + 8]
add    rax, [rsp]
add    rsp, 16
mov    rsp, rbp
pop    rbp
ret
mov    rsp, rbp
pop    rbp
ret
_main:
push   rbp
mov    rbp, rsp

sub    rsp, 8
mov    rax, 8
push   rax
call   _fib
add    rsp, 16
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rsp, rbp
pop    rbp
ret
