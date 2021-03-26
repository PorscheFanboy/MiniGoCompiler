BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
w dq 0
section .text
global _main
_main:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 1
mov    [rbp-16], rax

.for_loop0:
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 9
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_1
mov    rax, 0
jmp    .if_lessdone1
.if_less_1:
mov    rax, 1
.if_lessdone1:
add    rsp, 16

cmp    rax, 0
je     .for_done0
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    [rsp], rax
push   rbx
sub    rsp, 16
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem2
mov    rdi, 8000
call   _malloc
mov    qword [rax], 0
.add_elem2:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
.for_post0:
mov    rax, rbp
sub    rax, 16

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    .for_loop0
.for_done0:
sub    rsp, 8
lea    rax, [w]

mov    [rsp], rax
push   rbx
sub    rsp, 16
lea    rax, [w]

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem4
mov    rdi, 8000
call   _malloc
mov    qword [rax], 0
.add_elem4:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

.for_loop5:
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 6
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_6
mov    rax, 0
jmp    .if_lessdone6
.if_less_6:
mov    rax, 1
.if_lessdone6:
add    rsp, 16

cmp    rax, 0
je     .for_done5
sub    rsp, 8
sub    rsp, 8
lea    rax, [w]

mov    rax, [rax]

mov    [rsp], rax
mov    rax, 0

add    rax, 1
imul   rax, 8
add    rax, [rsp]
mov    rax, [rax]
add    rsp, 8

mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
mov    rax, [rax]
add    rsp, 8
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

.for_post5:
mov    rax, rbp
sub    rax, 16

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    .for_loop5
.for_done5:
sub    rsp, 16
mov    rax, 999
mov    qword [rsp + 8], rax
sub    rsp, 8
sub    rsp, 8
lea    rax, [w]

mov    rax, [rax]

mov    [rsp], rax
mov    rax, 0

add    rax, 1
imul   rax, 8
add    rax, [rsp]
mov    rax, [rax]
add    rsp, 8

mov    [rsp], rax
mov    rax, 4

add    rax, 1
imul   rax, 8
add    rax, [rsp]
mov    rax, [rax]
add    rsp, 8
mov    rcx, rax
mov    rax, [rsp + 8]
idiv   rcx
mov    rax, rdx
add    rsp, 16
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rsp, rbp
pop    rbp
ret
