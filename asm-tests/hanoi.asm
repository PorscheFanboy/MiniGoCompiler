BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
count dq 0
section .text
global _main
_towers:
push   rbp
mov    rbp, rsp

.ifcond_0:
sub    rsp, 16
mov    rax, rbp
add    rax, 40

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
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
je     if_done_0
lea    rax, [count]

mov    rbx, rax
sub    rsp, 8
lea    rax, [count]

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
mov    rsp, rbp
pop    rbp
ret
if_done_0:
sub    rsp, 16
mov    rax, rbp
add    rax, 40

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
push   rax
mov    rax, [rbp+32]
push   rax
mov    rax, [rbp+16]
push   rax
mov    rax, [rbp+24]
push   rax
call   _towers
add    rsp, 32

lea    rax, [count]

mov    rbx, rax
sub    rsp, 8
lea    rax, [count]

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
sub    rsp, 16
mov    rax, rbp
add    rax, 40

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
push   rax
mov    rax, [rbp+16]
push   rax
mov    rax, [rbp+24]
push   rax
mov    rax, [rbp+32]
push   rax
call   _towers
add    rsp, 32

mov    rsp, rbp
pop    rbp
ret
_main:
push   rbp
mov    rbp, rsp

mov    rax, 28
push   rax
mov    rax, 65
push   rax
mov    rax, 67
push   rax
mov    rax, 66
push   rax
call   _towers
add    rsp, 32

lea    rax, [count]

mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rsp, rbp
pop    rbp
ret
