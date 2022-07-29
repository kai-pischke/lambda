# Lambda Calculus Calculator

## About

A toy calculator for playing with the [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) I made a while ago. Uses de Bruijn representation of lambda terms internally to avoid [α-headaches](https://www.youtube.com/watch?v=IC7pZSd8iRg). Written in Haskell. Still very much incomplete and subject to whatever changes I feel like making.

## Usage

### Display a term :mag_right: 
The term will be silently [α-converted](https://en.wikipedia.org/wiki/Lambda_calculus) so that variables are chosen in alphabetical order.
```
>> (λxy.x)(λx.x)
(λab.a)(λa.a)
```

### Perform leftmost reduction :point_left:
Use the `:leftmost` or `:lm` command to perform leftmost reduction.
```
>> :leftmost (λxy.x)(λx.x)
λab.b
>> :lm (λx.xx)(λx.xx)
(λa.aa)(λa.aa)
```
You can't leftmost reduce something which is in [β-normal form](https://en.wikipedia.org/wiki/Beta_normal_form).
```
>> :lm (λx.xx)
(β-normal) λa.aa
```

### Find the β-normal form :innocent:
Use `:betanormal` or `:bn` to find the [β-normal form](https://en.wikipedia.org/wiki/Beta_normal_form).
```
>> :bn (λx.xx)(λx.x)
λa.a
```

### Convert to DeBruijn notation :microscope:
Use `:debruijn` or `:db` to view the [De Bruijn notation](https://en.wikipedia.org/wiki/De_Bruijn_notation).
```
>> :debruijn (λx.xx)(λx.xx)
(λ 0 0) (λ 0 0)
>> :db (λx.xx)(λxyz.xz)(λx.xx)
(λ 0 0) (λ λ λ 2 0) (λ 0 0)
```

## Installation 
Compile with `ghc -O2 main.hs -o lambda` and run `./lambda`. It is often a good idea to run it with [rlwrap](https://linux.die.net/man/1/rlwrap) (use `rlwrap ./lambda`) so you can edit what you write.
