BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
chessGridSize dq 27
queens dq 0
section .text
global _main
_queenRecursion:
push   rbp
mov    rbp, rsp

.ifcond_0:
sub    rsp, 16
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
lea    rax, [chessGridSize]

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jge    .if_less_1
mov    rax, 0
jmp    .if_lessdone1
.if_less_1:
mov    rax, 1
.if_lessdone1:
add    rsp, 16

cmp    rax, 0
je     if_done_0
mov    rax, 1
mov    rsp, rbp
pop    rbp
ret
if_done_0:
sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

sub    rsp, 16
mov    rdi, 16
call   _malloc
mov    [rbp-24], rax
sub    rsp, 16
mov    rax, 0
mov    [rbp-32], rax

sub    rsp, 16
mov    rdi, 16
call   _malloc
mov    [rbp-40], rax
sub    rsp, 16
mov    rax, 0
mov    [rbp-48], rax

sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    [rsp], rax
sub    rsp, 8
lea    rax, [queens]

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
add    rax, 16

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
sub    rax, 8

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_loop2:
sub    rsp, 16
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp + 8], rax
lea    rax, [chessGridSize]

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
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 0

mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 32

mov    [rsp], rax
mov    rax, 1

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_loop4:
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 16

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
je     for_done4
sub    rsp, 8
mov    rax, rbp
sub    rax, 40

mov    [rsp], rax
sub    rsp, 8
lea    rax, [queens]

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
.ifcond_6:
sub    rsp, 16
sub    rsp, 16
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_7
mov    rax, 0
jmp    .if_done7
.if_true_7:
mov    rax, 1
.if_done7:
add    rsp, 16
mov    qword [rsp + 8], rax
sub    rsp, 16
sub    rsp, 16
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 8

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
mov    qword [rsp + 8], rax
sub    rsp, 16
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_8
mov    rax, 0
jmp    .if_done8
.if_true_8:
mov    rax, 1
.if_done8:
add    rsp, 16
mov    qword [rsp], rax
mov    rax, [rsp + 8]
or     rax, [rsp]
add    rsp, 16
mov    qword [rsp + 8], rax
sub    rsp, 16
sub    rsp, 16
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 8

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
mov    qword [rsp + 8], rax
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_10
mov    rax, 0
jmp    .if_done10
.if_true_10:
mov    rax, 1
.if_done10:
add    rsp, 16
mov    qword [rsp], rax
mov    rax, [rsp + 8]
or     rax, [rsp]
add    rsp, 16

cmp    rax, 0
je     if_done_6
sub    rsp, 8
mov    rax, rbp
sub    rax, 32

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
jmp    for_done4
if_done_6:
for_post4:
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
jmp    for_loop4
for_done4:
.ifcond_12:
sub    rsp, 16
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_13
mov    rax, 0
jmp    .if_done13
.if_true_13:
mov    rax, 1
.if_done13:
add    rsp, 16

cmp    rax, 0
je     if_done_12
sub    rsp, 8
sub    rsp, 8
lea    rax, [queens]

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

.ifcond_14:
sub    rsp, 16
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8
push   rax
call   _queenRecursion
add    rsp, 16
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_15
mov    rax, 0
jmp    .if_done15
.if_true_15:
mov    rax, 1
.if_done15:
add    rsp, 16

cmp    rax, 0
je     if_done_14
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 0

mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rax, rbp
sub    rax, 24

mov    rax, [rax]

add    rax, 8

mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

sub    rsp, 8
mov    rax, rbp
sub    rax, 48

mov    [rsp], rax
mov    rax, 1

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
jmp    for_done2
if_done_14:
if_done_12:
for_post2:
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
jmp    for_loop2
for_done2:
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]
mov    rsp, rbp
pop    rbp
ret
mov    rsp, rbp
pop    rbp
ret
_main:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_loop16:
sub    rsp, 16
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp + 8], rax
lea    rax, [chessGridSize]

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_17
mov    rax, 0
jmp    .if_lessdone17
.if_less_17:
mov    rax, 1
.if_lessdone17:
add    rsp, 16

cmp    rax, 0
je     for_done16
sub    rsp, 16
mov    rdi, 16
call   _malloc
mov    [rbp-16], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 0

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 8

mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
lea    rax, [queens]

mov    [rsp], rax
push   rbx
sub    rsp, 16
lea    rax, [queens]

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem18
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem18:
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
for_post16:
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
jmp    for_loop16
for_done16:
sub    rsp, 8
mov    rax, 0
push   rax
call   _queenRecursion
add    rsp, 16

mov    rsp, rbp
pop    rbp
ret
