cassowary-haskell
=================

[![Build Status](https://travis-ci.org/athanclark/cassowary-haskell.svg?branch=master)](https://travis-ci.org/athanclark/cassowary-haskell)

> An implementation of the Cassowary linear constraint solver, in Haskell

## Installation

```bash
git clone https://github.com/athanclark/cassowary-haskell.git
cd cassowary-haskell
cabal sandbox init && cabal install
```

### How to run tests

```bash
cabal install --enable-tests && cabal test --show-details=always
```

## Bird's Eye View

Linear Program Solvers are fairly complex, but in short it is something like a
compiler for linear equations - "[standard form](https://en.wikipedia.org/wiki/Linear_equation#General_.28or_standard.29_form)"
becomes our grammar / AST for the language, and the act of compilation is the
"constraint solving" technique we employ. In this instance, we are using the
[Cassowary](https://constraints.cs.washington.edu/solvers/cassowary-tochi.pdf)
LP solver.

There are a few components to the system:

- [The Grammar](#the-grammar) for Linear Equations
- [The Simplex Method](#the-simplex-method)
- [Handling Unrestricted Variables](#unrestricted-variables)
- [Handling Weighted Constraints](#weights)

### The Grammar

There is a user-facing syntax to be used as a DSL when creating equations, and
an internal grammar representing linear equations in standard form. At both
layers, linearity of the equations is guaranteed by design.

#### User-Facing API

There are a few operators to consider when writing equations - `.+.`, `.*.`,
`.==.`, `.<=.` and `.>=.`. To create a variable, you need to wrap your string name
in a `EVar` constructor, and sometimes you will need to coerce numeric literals
to `(8 :: Rational)`. Also, sometimes you may need to wrap numbers with `ELit`.
Otherwise, you write your equations like normal:

`5x - 3y + 2z = 0` becomes

```haskell
equ1 = (5 :: Double) .*. EVar "x"
   .+. (3 :: Double) .*. EVar "y"
   .+. (2 :: Double) .*. EVar "z"
  .==. ELit 0
```

This will create a value of type `IneqExpr` (expressions representing inequalities).
To see what this looks like in standard form, use `standardForm`:

```haskell
equ1std :: IneqStdForm
equ1std = standardForm equ1

λ> equ1std
➥ EquStd [LinVar 5 "x", LinVar 3 "y", LinVar 2 "z"] 0
```

An equation (via `EquStd`) consists of:

- A list of variables with their coefficients (`LinVar`), implicitly added together
- A constant value (in this case, `0`)

#### Standard Form

A linear equation in standard form is when all variables and constant values
are separated between their inequality operator. Our example above is already in standard
form, but for demonstration here is another:

`3x + 4y - 1 <= 3z + 2` becomes

`3x + 4y - 3z <= 3`

The internal grammatical data structure sees this as a list of variables with their
coefficients summed together, with the constant - a structure isomorphic to `([(String, Rational)], Rational)`
(where `LinVar` is isomorphic to a tuple `(,)`, and `EquStd` is also isomorphic to a
tuple `(,)`)
for each inequality operator. This makes everything much more simple to work with
in the compiler.

> Our implementation actually uses `Map String Rational` for log(n) lookups, and
> deletes `0` coefficients to reduce space.

### The Simplex Method

This is where we compile. The first thing we need to understand is a constraint set /
tableau. This is where we store all the equations - we could be primitive and just make
a list of equations:

```haskell
newtype LameTableau = LameTableau [IneqStdForm]
```

but this neglegts what simplex actually does to solve equations - it refines the set of basic-normal
form variables in the constraint set successively, through pivots.

#### Basic Normal Form

An equation with a variable in BNF should be the **only** equation
with that variable - "basic" variables uniquely identify equations in the set.
Simplex successively solves the system of equations by (efficiently) refactoring the constraint set,
making new basic variables with each pivot. In effect, simplex is isolating _definitions_ for those
basic variables.

At the same time, it's successively finding a more optimal basic-feasible solution - a solution where
each basic variable is assigned the constant in their corrosponding definition; ie: all other variables
in the definition are set to `0`. This is how (primal) simplex
finds the optimal (maximum) solution for a constraint set: It refactors the set, creating
basic variables (via Bland's rule), then takes the simplest substitution - each constant as the value for the
basic variables, and all others become zero. This actually "walks" the feasible region's corners
on each pivot, and does indeed find the most optimal solution as the most simple one.

We refine the tableau data structure to optimize for this basic feasible solution, accommodating
a separate Map for equations in basic-normal form:

```haskell
data Tableau = Tableau (Map LinVarName IneqStdForm) [IneqStdForm]
```

Each pivot then moves an equation from the list of constraints to the Map (if it can).

#### Slack Variables

Simplex can only work on equality constraints (`.==.`) - we need a way to turn an inequality into an
equality, soundly. That's what "slack" variables are used for:

`3x + 2y <= 20` becomes

`3x + 2y + s1 == 20` where `s1 >= 0`

We do this with a simple recursive function over the constraint list - a stateful
monad that makes sure no duplicate names are used.

#### Dual Simplex

Cassowary aims to _minimize_ changes, while primal simplex tries to _maximize_ variables
in a constraint set. Therefore, we need to take the dual approach to simplex, which
is identical to the primal simplex enacted on the transpose of the tableau.

We instead create a dual Bland-rule, which chooses the next basic variable and definition.
This saves us the costly overhead of a transpose.

### Unrestricted Variables

In Cassowary, we expect to reduce constraint sets for equations involving
_unrestricted_ variables - variables that can be positive or negative. The simplex
method, by default, only works on positive variables, so we need to account for this.

We can't use simplex on unrestricted variables, only on positive variables of the
form `>= 0`. So, we make a mapping `x = err_x_+ - err_x_-`, where `err_x_+, err_x_- >= 0`.
This means we can take any number, and separate it into it's positive and negative components.
When we use these equations in Cassowary, they're seen as the _change_ in the values
for each constraint. We want to minimize these as much as possible, and can be seen
as error metrics. We then substitute `err_x_+ - err_x_-` for `x` in every equation.
Now, we can use the simplex method on these restricted equations.

### Weights

Bland's rule is the technique the simplex method uses when deciding which variable should be chosen
as the next basic variable to pivot on. Basically, it's theory is that the varaible in the objective
function with the "most negative" effect should be made as the next quantity to isolate.

However, we want weights for our constraints, choosing stronger constraints over
weaker ones, in a method we declare.

At the user-level, we do this with a natural-number analogue - `0` represents required constraints,
and more positive represents added weakness to the equation. We can add this weight
in a constraint-by-constraint basis:

```haskell
equ1weighted = addWeight 3 equ1std
```

However, this means we need to encode the "weight" meaning in each constraint, and each variable.
We do this by translating the natural numbers into non-empty
inductive lists full of coefficients for each weight, making the list `n` long. When adding weight
to a constraint, we generate `0`s until `n`, which holds
the original coefficient. So, the result of `addWeight 3 equ1std` would be

```haskell
equ1weighted = EquStd [ ("x", [0,0,0,5])
                      , ("y", [0,0,0,3])
                      , ("z", [0,0,0,2])
                      ] 0
```

The unfortunate side-effect of this design is the resulting arithmetic operators `+`,`-`,`*`, and `/`.
Addition and substitution are pretty simple - you take the union of the coefficients, and combining their
contents with `+` or `-` when they collide. The more difficult area is multiplication and division -
we can multiply a `Rational * [Rational]` pretty easily via distribution / `fmap`, but division `Rational / [Rational]`
is unintuitive - we actually take the sum of all coefficients to obtain a `Rational / Rational`.

## Conclusion

These are the more important conecpts to understand before diving into the code. Please let me know if
you need clarification!
