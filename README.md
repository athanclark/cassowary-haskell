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

## Docs

The system is fairly complex, but in short it is something like a compiler for
linear equations. There are a few components to the system:

- The Grammar for Linear Equations
- The Simplex Method
- Handling Unrestricted Variables
- Handling Weighted Constraints

### The Grammar

There is a user-facing grammar, to be used as a DSL when creating equations, and
an internal data type representing linear equations in standard form. At both
layers, linearity is guaranteed in the type system.

#### User-Facing API

There are a few operators to consider when writing equations - `.+.`, `.*.`,
`.==.`, `.<=.` and `.>=.`. To create a variable, you need to wrap your string name
in a `EVar` constructor, and sometimes you will need to coerce numeric literals
to `(8 :: Rational)`. Also, sometimes you may need to wrap numbers with `ELit`.
Otherwise, you write your equations just like you normally would:

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
```

#### Standard Form

A linear equation in standard form is just the case when all variables and constants
are segregated between their inequality operator. Our case above is already in standard
form, but just as an example:

`3x + 4y - 1 <= 3z + 2` becomes

`3x + 4y - 3z <= 3`

Our grammatical data structure sees this as simply a list of varaibles with their
coefficients, and the constant - isomorphic to `([(String, Rational)], Rational)`
for each inequality operator. This makes everything much more simple to work with.

> Our implementation actually uses `Map String Rational` for log(n) lookups, and
> deletes `0` coefficients for less space.

### The Simplex Method

This is where we compile our data. The first thing we need to understand is a constraint set /
tableau. In Cassowary, we expect to reduce constraint sets for equations envolving
_unrestricted_ variables - variables that can be positive or negative. Simplex by
default only works on positive variables, so we need to take care of this. As a
naive start, we could set a tableau to just be a list of equations in standard form:

```haskell
newtype Tableau' = Tableau' [IneqStdForm]
```

but this neglegts what simplex does to solve equations - it refines the basic-normal
form of the constraint set successively, through pivots.

#### Basic Normal Form

An equation with a basic variable in a constraint set should be the only equation
with that variable - "basic" variables are just unique in the set. Simplex successively
solves the system of equations by refactoring the constraint set, making new basic
variables with each pivot, isolating definitions for those basic varaibles. At the
same time, it's finding a successively more optimal basic-feasible solution, where
each basic varaible is assigned the constant in their equation. This is how (primal) simplex
finds the optimal (maximum) solution for a constraint set: It refactors the set, creating
basic variables, then takes the simplest substitution - each constant as the value for the
basic variables, and all others become zero. This "walks" the feasible region's corners
on each pivot.

We refine the tableau data structure to optimize for this basic feasible solution, with
a separate Map for equations in basic-normal form:

```haskell
data Tableau = Tableau (Map LinVarName IneqStdForm) [IneqStdForm]
```

Each pivot then moves an equation from the list of constraints to the Map (if it can).

#### Slack Variables

Simplex also can only work on equality constraints (`.==.`), therefore we need
to create spare variables to account for this:

`3x + 2y <= 20` becomes

`3x + 2y + s1 == 20` where `s1 >= 0`

We do this with a simple recursive function over the constraint list - a stateful
monad that makes sure no duplicate names are used.

#### Dual Simplex

Cassowary aims to _minimize_ changes, while primal simplex tries to _maximize_ variables
in a constraint set. Therefore, we need to take the dual approach to simplex, which
is primal simplex on the transpose of the dual.

To transpose a constrant set, we need to invert the tableau. In primal simplex, we
have an objective function (standard form linear equation) we want to maximize over a
constraint set, where each equation in the set has a unique slack varaible, uses a
common set of main variable names, and has a constant value. The inverse turns all
slack variables into common main variables, and leaves the old main variables as unique
slack variables in the new equation. Likewise, the constants for each equation become
the coefficients for each (slack) variable in the objective funciton, and those old
coefficients become the new constants for the transpose.

After you have a transposed tableau, then you just run simplex primal on it, then
transpose it again to get back the original shape.

### Unrestricted Variables

We can't use simplex on unrestricted variables, only on positive variables of the
form `>= 0`. So, we make a mapping `x = err_x_+ - err_x_-`, where `err_x_+, err_x_- >= 0`.
This means we can take any number, and separate it into it's positive and negative components.
When we use these equations in Cassowary, they're seen as the _change_ in the values
for each constraint. We want to minimize these as much as possible, and are therefore seen
as error metrics. We then substitute `err_x_+ - err_x_-` for `x` in every equation.
Now, we can use simplex.

### Weights

Bland's rule is the technique simplex uses when deciding which variable should be chosen
as the next basic variable to pivot on. Basically, it's theory is that the "most negative"
coefficient in the objective function should be made as the next corner to walk on.

However, we want weights for our constraints, chosing more strong constraints over
weaker ones, in a method we declare.

We do this with a natural-number analogue - `0` represents required constraints,
and more positive represents added weakness to the equation. we can add this weight
in a constraint-by-constraint basis, in total:

```haskell
equ1weighted = addWeight 3 equ1std
```

But, now this means we need to encode that meaning in each constraint, and each variable
when we refactor equations. We do this by translating the natural numbers into
inductive lists, making the list `n` long (populated with `0`s), until `n` which holds
the coefficient. So, the result of `addWeight 3 equ1std` would be

```haskell
equ1weighted = EquStd [ ("x", [0,0,0,5])
                      , ("y", [0,0,0,3])
                      , ("z", [0,0,0,2])
                      ] 0
```

Now, whenever we add two coefficients for the same variable, we just do `unionWith (+)`.
This way, when we choose the next variable with Bland's rule, we are looking for the
most-required coefficients first before coming to a solution.
