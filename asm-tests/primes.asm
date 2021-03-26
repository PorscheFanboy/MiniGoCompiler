BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
section .text
global _main
_main:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 1000000
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    [rsp], rax
push   rbx
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem0
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem0:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
mov    rax, 2
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 16
mov    rax, 2
mov    [rbp-24], rax

sub    rsp, 16
mov    rax, 4
mov    [rbp-32], rax

sub    rsp, 16
mov    rax, 1
mov    [rbp-40], rax

sub    rsp, 16
mov    rax, 2
mov    [rbp-48], rax

sub    rsp, 16
mov    rax, 2
mov    [rbp-56], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-64], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-72], rax

sub    rsp, 16
mov    rax, 1
mov    [rbp-80], rax

sub    rsp, 16
mov    rax, 1
mov    [rbp-88], rax

for_loop1:

sub    rsp, 16
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_2
mov    rax, 0
jmp    .if_lessdone2
.if_less_2:
mov    rax, 1
.if_lessdone2:
add    rsp, 16

cmp    rax, 0
je     for_done1
mov    rax, rbp
sub    rax, 48

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
.ifcond_3:
sub    rsp, 16
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jg     .if_less_4
mov    rax, 0
jmp    .if_lessdone4
.if_less_4:
mov    rax, 1
.if_lessdone4:
add    rsp, 16

cmp    rax, 0
je     if_done_3
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
sub    rsp, 8
mov    rax, rbp
sub    rax, 32

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
imul   rax, [rsp]
add    rsp, 8

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
if_done_3:
sub    rsp, 8
mov    rax, rbp
sub    rax, 80

mov    [rsp], rax
mov    rax, 1

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 64

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_loop5:
sub    rsp, 16
mov    rax, rbp
sub    rax, 64

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 88

mov    rax, [rax]
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
je     for_done5
sub    rsp, 8
mov    rax, rbp
sub    rax, 72

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 64

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
.ifcond_7:
sub    rsp, 16
mov    rax, rbp
sub    rax, 72

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jg     .if_less_8
mov    rax, 0
jmp    .if_lessdone8
.if_less_8:
mov    rax, 1
.if_lessdone8:
add    rsp, 16

cmp    rax, 0
je     if_done_7
jmp    for_done5
if_done_7:
.ifcond_9:
sub    rsp, 16
push   rcx
push   rdx
sub    rsp, 8
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]
mov    dword [rsp], eax
mov    rax, rbp
sub    rax, 72

mov    rax, [rax]
mov    ecx, eax
mov    eax, dword [rsp]
xor    rdx, rdx
idiv   ecx
xor    rax, rax
mov    eax, edx
add    rsp, 8
pop    rdx
pop    rcx
mov    qword [rsp + 8], rax
mov    rax, 0
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

cmp    rax, 0
je     if_done_9
sub    rsp, 8
mov    rax, rbp
sub    rax, 80

mov    [rsp], rax
mov    rax, 0

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
jmp    for_done5
if_done_9:
for_post5:
mov    rax, rbp
sub    rax, 64

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 64

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
jmp    for_loop5
for_done5:
.ifcond_11:
sub    rsp, 16
mov    rax, rbp
sub    rax, 80

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
je     .if_true_12
mov    rax, 0
jmp    .if_done12
.if_true_12:
mov    rax, 1
.if_done12:
add    rsp, 16

cmp    rax, 0
je     if_done_11
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    [rsp], rax
push   rbx
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem13
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem13:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
mov    rax, rbp
sub    rax, 88

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 88

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 56

mov    [rsp], rax
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
mov    rax, rbp
sub    rax, 40

mov    rbx, rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
if_done_11:
jmp for_loop1
for_done1:
mov    rax, rbp
sub    rax, 56

mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rsp, rbp
pop    rbp
ret
