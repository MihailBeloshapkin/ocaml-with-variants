Copyright 2021-2022, Mihail Beloshapkin
SPDX-License-Identifier: CC0-1.0

Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct


  $ ./simpleFuncs.exe <<-EOF
  > 2 + 3
  > EOF
  5 

  $ ./simpleFuncs.exe <<-EOF
  > 2 = 3
  > EOF
  false 

  $ ./simpleFuncs.exe <<-EOF
  > 3 = 3
  > EOF
  true 

