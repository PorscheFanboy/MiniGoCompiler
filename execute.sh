#!/bin/bash
#
# Execute the generated code
#
# This script takes in the filename of the ORIGINAL program AFTER the codegen has been run
#   i.e. programs/3-semantics+codegen/valid/test.min
#
# It MUST then
#   (a) Compile the GENERATED file
#         i.e. programs/3-semantics+codegen/valid/test.cpp
#   (b) Execute the compiled code
#
# (if no compilation is needed, then only perform step b)
#
# To conform with the verification script, this script MUST:
#   (a) Output ONLY the execution
#   (b) Exit with status code 0 for success, not 0 otherwise

# You MUST replace the following line with the command to execute your compiled code
# Note the bash replacement which changes:
#   programs/3-semantics+codegen/valid/test.min -> programs/3-semantics+codegen/valid/test.out
# python2 ${1%.*}.py
./run.sh codegen ${1%.*}.go
nasm -g -f macho64 ${1%.*}.asm && ld -lc -o ${1%.*} ${1%.*}.o -macosx_version_min 10.13 -lSystem
./${1%.*}
# Lastly, we propagate the exit code
# exit $?
