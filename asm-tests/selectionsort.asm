BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
section .text
global _main
_selection_sort:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

for_loop0:
sub    rsp, 16
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
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
je     for_done0
sub    rsp, 16
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
mov    [rbp-16], rax

sub    rsp, 16
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    [rbp-24], rax

sub    rsp, 16
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8
mov    [rbp-32], rax

for_loop2:
sub    rsp, 16
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_3
mov    rax, 0
jmp    .if_lessdone3
.if_less_3:
mov    rax, 1
.if_lessdone3:
add    rsp, 16

cmp    rax, 0
je     for_done2
.ifcond_4:
sub    rsp, 16
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_5
mov    rax, 0
jmp    .if_lessdone5
.if_less_5:
mov    rax, 1
.if_lessdone5:
add    rsp, 16

cmp    rax, 0
je     if_done_4
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    [rsp], rax
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
if_done_4:
for_post2:
mov    rax, rbp
sub    rax, 32

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    for_loop2
for_done2:
.ifcond_6:
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jne     .if_true_7
mov    rax, 0
jmp    .if_done7
.if_true_7:
mov    rax, 1
.if_done7:
add    rsp, 16

cmp    rax, 0
je     if_done_6
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

if_done_6:
for_post0:
mov    rax, rbp
sub    rax, 8

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    for_loop0
for_done0:
mov    rsp, rbp
pop    rbp
ret
_main:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 200
mov    [rbp-16], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-24], rax

for_loop8:
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_9
mov    rax, 0
jmp    .if_lessdone9
.if_less_9:
mov    rax, 1
.if_lessdone9:
add    rsp, 16

cmp    rax, 0
je     for_done8
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
jne    .add_elem10
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem10:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
sub    rsp, 16
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_post8:
mov    rax, rbp
sub    rax, 24

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    for_loop8
for_done8:
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
push   rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
push   rax
call   _selection_sort
add    rsp, 16

sub    rsp, 16
mov    rax, 0
mov    [rbp-24], rax

for_loop11:
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 10
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_12
mov    rax, 0
jmp    .if_lessdone12
.if_less_12:
mov    rax, 1
.if_lessdone12:
add    rsp, 16

cmp    rax, 0
je     for_done11
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

for_post11:
mov    rax, rbp
sub    rax, 24

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    for_loop11
for_done11:
mov    rsp, rbp
pop    rbp
ret
