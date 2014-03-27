#!/usr/bin/env sh

clisp -c algos.lsp
clisp -c create.lsp
clisp -c greedy.lsp
clisp -c io.lsp
clisp -c minmax.lsp
clisp -c moves.lsp
clisp -c test.lsp
clisp -c 3dttt.lsp
clisp 3dttt.fas
rm *.fas
rm *.lib
