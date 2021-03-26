BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
s dq 0
cost_s dq 0
w dq 0
r dq 0
section .text
global _main
_rand:
push   rbp
mov    rbp, rsp

sub    rsp, 8
lea    rax, [r]

mov    [rsp], rax
push   rcx
push   rdx
sub    rsp, 8
sub    rsp, 8
sub    rsp, 8
mov    rax, 8121
mov    qword [rsp], rax
lea    rax, [r]

mov    rax, [rax]
imul   rax, [rsp]
add    rsp, 8
mov    qword [rsp], rax
mov    rax, 28411
add    rax, [rsp]
add    rsp, 8
mov    dword [rsp], eax
mov    rax, 134456
mov    ecx, eax
mov    eax, dword [rsp]
xor    rdx, rdx
idiv   ecx
xor    rax, rax
mov    eax, edx
add    rsp, 8
pop    rdx
pop    rcx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
lea    rax, [r]

mov    rax, [rax]
mov    rsp, rbp
pop    rbp
ret
mov    rsp, rbp
pop    rbp
ret
_compute_cost:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

for_loop0:
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
sub    rsp, 16
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
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
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
sub    rsp, 8
sub    rsp, 8
lea    rax, [w]

mov    rax, [rax]

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 16

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

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 16

mov    rax, [rax]

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
add    rax, [rsp]
add    rsp, 8

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_post0:
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
jmp    for_loop0
for_done0:
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
sub    rsp, 8
sub    rsp, 8
lea    rax, [w]

mov    rax, [rax]

mov    [rsp], rax
sub    rsp, 16
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]

mov    [rsp], rax
mov    rax, 0

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
add    rax, [rsp]
add    rsp, 8
mov    rsp, rbp
pop    rbp
ret
mov    rsp, rbp
pop    rbp
ret
_permute:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

for_loop2:
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 24

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
sub    rax, 8

mov    [rsp], rax
push   rbx
sub    rsp, 16
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem4
mov    rdi, 88888888
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
mov    rax, 0
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_post2:
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
jmp    for_loop2
for_done2:
sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-24], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-32], rax

sub    rsp, 16
mov    rax, 1
mov    [rbp-40], rax

for_loop5:
sub    rsp, 16
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 24

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
.ifcond_7:
sub    rsp, 16
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_8
mov    rax, 0
jmp    .if_lessdone8
.if_less_8:
mov    rax, 1
.if_lessdone8:
add    rsp, 16

cmp    rax, 0
je     if_else7
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    [rsp], rax
sub    rsp, 8
push   rcx
push   rdx
sub    rsp, 8
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]
mov    dword [rsp], eax
mov    rax, 2
mov    ecx, eax
mov    eax, dword [rsp]
xor    rdx, rdx
idiv   ecx
xor    rax, rax
mov    eax, edx
add    rsp, 8
pop    rdx
pop    rcx
mov    qword [rsp], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
imul   rax, [rsp]
add    rsp, 8

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 16

mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 16

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

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 16

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
add    rax, 16

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

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
add    rax, 16

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

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

sub    rsp, 8
mov    rax, rbp
sub    rax, 32

mov    [rsp], rax
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
push   rax
call   _compute_cost
add    rsp, 16

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
.ifcond_9:
sub    rsp, 16
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]
mov    qword [rsp + 8], rax
lea    rax, [cost_s]

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_10
mov    rax, 0
jmp    .if_lessdone10
.if_less_10:
mov    rax, 1
.if_lessdone10:
add    rsp, 16

cmp    rax, 0
je     if_done_9
sub    rsp, 8
lea    rax, [cost_s]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
sub    rsp, 16
mov    rax, 0
mov    [rbp-48], rax

for_loop11:
sub    rsp, 16
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]
mov    qword [rsp + 8], rax
sub    rsp, 16
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
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
sub    rsp, 8
lea    rax, [s]

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 48

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 16

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 48

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

for_post11:
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
jmp    for_loop11
for_done11:
if_done_9:
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8

mov    rbx, rax
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8

mov    [rbx], rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 40

mov    [rsp], rax
mov    rax, 1

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
jmp    if_done_7
if_else7:
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
mov    rax, 0
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
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
if_done_7:
for_post5:

jmp    for_loop5
for_done5:
mov    rsp, rbp
pop    rbp
ret
_main:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 11
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

for_loop13:
sub    rsp, 16
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_14
mov    rax, 0
jmp    .if_lessdone14
.if_less_14:
mov    rax, 1
.if_lessdone14:
add    rsp, 16

cmp    rax, 0
je     for_done13
sub    rsp, 8
lea    rax, [s]

mov    [rsp], rax
push   rbx
sub    rsp, 16
lea    rax, [s]

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem15
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem15:
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
sub    rsp, 16
mov    rax, 0
mov    [rbp-24], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-32], rax

for_loop16:
sub    rsp, 16
mov    rax, rbp
sub    rax, 32

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
sub    rax, 8

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
sub    rsp, 8
mov    rax, rbp
sub    rax, 24

mov    [rsp], rax
push   rbx
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

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
call   _rand
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
jmp    for_loop16
for_done16:
sub    rsp, 8
lea    rax, [w]

mov    [rsp], rax
push   rbx
sub    rsp, 16
lea    rax, [w]

mov    rax, [rax]
cmp    rax, 0
jne    .add_elem19
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem19:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    rbx, [rsp]
mov    [rbx], rax
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_post13:
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
jmp    for_loop13
for_done13:
sub    rsp, 8
lea    rax, [cost_s]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
push   rax
lea    rax, [s]

mov    rax, [rax]
push   rax
call   _compute_cost
add    rsp, 16

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
lea    rax, [cost_s]

mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
push   rax
lea    rax, [s]

mov    rax, [rax]
push   rax
call   _permute
add    rsp, 16

lea    rax, [cost_s]

mov    rax, [rax]
mov    rsi, rax
xor    rax, rax
lea    rdi, [int_str]
call   _printf

mov    rsp, rbp
pop    rbp
ret
