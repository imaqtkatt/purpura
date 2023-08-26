# Purpura

Purpura is a statically typed functional programming language, highly influenced by Rust and Erlang.

## Goals

The main goal of purpura lang is to be compiled into BEAM Bytecode, JavaScript or LLVM.

## Code Example

```purr
data Nat {
  Zero(),
  Succ(Nat),
}

sig succ (Nat) -> Nat

fn succ (Zero())  = Succ(Zero())
fn succ (Succ(s)) = Succ(succ(s))
```
