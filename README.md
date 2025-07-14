# Purpura

Purpura is a statically typed functional programming language!

## Code Example

```purr
data Nat {
  Zero(),
  Succ(Nat),
}

sig succ : Nat -> Nat

def succ nat =
  match nat case {
    Zero() => Succ(Zero()),
    Succ(pred) => Succ(succ pred),
  }
```
