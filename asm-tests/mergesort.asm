BITS 64
DEFAULT REL
extern _puts, _printf, _malloc, _free
section .data
int_str db "%lld", 10, 0
section .text
global _main
_merge:
push   rbp
mov    rbp, rsp

sub    rsp, 16
push   rcx
push   rdx
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
add    rax, [rsp]
add    rsp, 8
mov    dword [rsp], eax
mov    rax, 2
mov    ecx, eax
mov    eax, dword [rsp]
xor    rdx, rdx
idiv   ecx
add    rsp, 8
pop    rdx
pop    rcx
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    [rbp-16], rax

sub    rsp, 16
mov    rax, rbp
add    rax, 24

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

for_loop0:

sub    rsp, 16
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
jle    .if_less_1
mov    rax, 0
jmp    .if_lessdone1
.if_less_1:
mov    rax, 1
.if_lessdone1:
add    rsp, 16
mov    qword [rsp + 8], rax
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
jle    .if_less_2
mov    rax, 0
jmp    .if_lessdone2
.if_less_2:
mov    rax, 1
.if_lessdone2:
add    rsp, 16
mov    qword [rsp], rax
mov    rax, [rsp + 8]
and    rax, [rsp]
add    rsp, 16

cmp    rax, 0
je     for_done0
.ifcond_4:
sub    rsp, 16
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
mov    qword [rsp + 8], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
je     if_else4
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 32

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

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
jmp    if_done_4
if_else4:
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 32

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

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
if_done_4:
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
jmp for_loop0
for_done0:
for_loop6:

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
jle    .if_less_7
mov    rax, 0
jmp    .if_lessdone7
.if_less_7:
mov    rax, 1
.if_lessdone7:
add    rsp, 16

cmp    rax, 0
je     for_done6
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 32

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

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
jmp for_loop6
for_done6:
for_loop8:

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
jle    .if_less_9
mov    rax, 0
jmp    .if_lessdone9
.if_less_9:
mov    rax, 1
.if_lessdone9:
add    rsp, 16

cmp    rax, 0
je     for_done8
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 32

mov    rax, [rax]

mov    [rsp], rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]

add    rax, 1
imul   rax, 8
add    rax, [rsp]
add    rsp, 8
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
mov    rbx, rax
mov    rax, [rsp]
mov    [rax], rbx
add    rsp, 8

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
jmp for_loop8
for_done8:
sub    rsp, 16
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    [rbp-40], rax

for_loop10:
sub    rsp, 16
mov    rax, rbp
sub    rax, 40

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jle    .if_less_11
mov    rax, 0
jmp    .if_lessdone11
.if_less_11:
mov    rax, 1
.if_lessdone11:
add    rsp, 16

cmp    rax, 0
je     for_done10
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 40

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
sub    rsp, 8
mov    rax, rbp
add    rax, 32

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

for_post10:
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
jmp    for_loop10
for_done10:
mov    rsp, rbp
pop    rbp
ret
_merge_sort_helper:
push   rbp
mov    rbp, rsp

.ifcond_12:
sub    rsp, 16
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jge    .if_less_13
mov    rax, 0
jmp    .if_lessdone13
.if_less_13:
mov    rax, 1
.if_lessdone13:
add    rsp, 16

cmp    rax, 0
je     if_done_12
mov    rsp, rbp
pop    rbp
ret
if_done_12:
sub    rsp, 16
push   rcx
push   rdx
sub    rsp, 8
sub    rsp, 8
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
add    rax, [rsp]
add    rsp, 8
mov    dword [rsp], eax
mov    rax, 2
mov    ecx, eax
mov    eax, dword [rsp]
xor    rdx, rdx
idiv   ecx
add    rsp, 8
pop    rdx
pop    rcx
mov    [rbp-8], rax

mov    rax, rbp
add    rax, 40

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 32

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
push   rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
push   rax
call   _merge_sort_helper
add    rsp, 32

mov    rax, rbp
add    rax, 40

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 32

mov    rax, [rax]
push   rax
sub    rsp, 8
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
mov    qword [rsp], rax
mov    rax, 1
add    rax, [rsp]
add    rsp, 8
push   rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
push   rax
call   _merge_sort_helper
add    rsp, 32

mov    rax, rbp
add    rax, 40

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 32

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
push   rax
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
push   rax
call   _merge
add    rsp, 32

mov    rsp, rbp
pop    rbp
ret
_merge_sort:
push   rbp
mov    rbp, rsp

sub    rsp, 16
mov    rax, 0
mov    [rbp-8], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-16], rax

for_loop14:
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
jl     .if_less_15
mov    rax, 0
jmp    .if_lessdone15
.if_less_15:
mov    rax, 1
.if_lessdone15:
add    rsp, 16

cmp    rax, 0
je     for_done14
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
jne    .add_elem16
mov    rdi, 88888888
call   _malloc
mov    qword [rax], 0
.add_elem16:
mov    [rsp+8], rax
mov    rbx, rax
mov    rax, [rax]
add    rax, 1
mov    [rbx], rax
imul   rax, 8
add    rax, rbx
mov    [rsp], rax
sub    rsp, 8
mov    rax, rbp
add    rax, 24

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
mov    rax, [rsp+8]
add    rsp, 16
pop    rbx

mov    rbx, [rsp]
mov    [rbx], rax
add    rsp, 8
for_post14:
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
jmp    for_loop14
for_done14:
mov    rax, rbp
add    rax, 24

mov    rax, [rax]
push   rax
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
push   rax
mov    rax, 0
push   rax
sub    rsp, 16
mov    rax, rbp
add    rax, 16

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 1
mov    qword [rsp], rax
mov    rax, [rsp + 8]
sub    rax, [rsp]
add    rsp, 16
push   rax
call   _merge_sort_helper
add    rsp, 32

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
mov    rax, 10000000
mov    [rbp-16], rax

sub    rsp, 16
mov    rax, 0
mov    [rbp-24], rax

for_loop17:
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
jl     .if_less_18
mov    rax, 0
jmp    .if_lessdone18
.if_less_18:
mov    rax, 1
.if_lessdone18:
add    rsp, 16

cmp    rax, 0
je     for_done17
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
for_post17:
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
jmp    for_loop17
for_done17:
mov    rax, rbp
sub    rax, 8

mov    rax, [rax]
push   rax
mov    rax, rbp
sub    rax, 16

mov    rax, [rax]
push   rax
call   _merge_sort
add    rsp, 16

sub    rsp, 16
mov    rax, 0
mov    [rbp-24], rax

for_loop20:
sub    rsp, 16
mov    rax, rbp
sub    rax, 24

mov    rax, [rax]
mov    qword [rsp + 8], rax
mov    rax, 10
mov    qword [rsp], rax
mov    rax, [rsp + 8]
cmp    rax, [rsp]
jl     .if_less_21
mov    rax, 0
jmp    .if_lessdone21
.if_less_21:
mov    rax, 1
.if_lessdone21:
add    rsp, 16

cmp    rax, 0
je     for_done20
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

for_post20:
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
jmp    for_loop20
for_done20:
mov    rsp, rbp
pop    rbp
ret
