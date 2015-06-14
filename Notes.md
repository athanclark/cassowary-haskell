Notes
=====

## End API

I'm going to adopt most of the user-level API from
[linear-grammar](http://hackage.haskell.org/package/linear-grammar), but subtle changes
will likely entail.

### Constraints

Cassowary constraints are weighted. These weights, at the user level, are just natural
numbers, where `0` is "required", and the larger one goes, the "weaker" the weight.
However, in the engine, we will need some type safety for the related weights. In the
paper, they detail a tuple-like encoding, as a way to segregate each error variable
and coefficient. This means we will likely need some form of inductive container to
handle the weight:

```haskell
data Weight a = Step Weight | Final a
```

where `0` would turn into `Final a` (where `a` is a coefficient or an error), and 
`2` would be `Step (Step (Final a))`. This is still up for debate.

Just like [simplex-basic](http://hackage.haskell.org/package/simplex-basic), we will
need to turn an inequality into an equality by creating slack variables, before entering
the data into the tableau matrix.

A constraint (at the user-level) _is_ an (in)equality:

```haskell
data IneqStdForm =
    EquStd [(String, Rational)] Rational (Weight ())
  | LteStd [(String, Rational)] Rational (Weight ())
  | GteStd [(String, Rational)] Rational (Weight ())

type Constraint = IneqStdForm
```

But, in the engine, we will need to turn each ineqality into equalities with slack variables
and error variables:

```
type SlackVars = [(String, Weight Rational)]

data ErrorVars = ErrorVars
  { errorPos :: Weight Rational
  , errorNeg :: Weight Rational
  }

data Row =
  Row [(String, Weight Rational)] (Weight Rational) SlackVars ErrorVars
```

### Immediate Result

Every tableau will have enough information in it to get the optimal substitution, where
we perform a dual simplex to minimize the positive & negative error metrics associated
with each constraint.

```haskell
getSubst :: Tableau -> Map String Rational
```

> This function should compute quickly

### Manipulation Methods

The Cassowary paper details three main operations - editing a constraint (which should
operate the fastest), adding a constraint, and deleting a constraint. If each constraint
is associated an `Integer` index, then we can use this to lens into a `Tableau`. Likewise,
we will need general leses (we'll start with routine records) for updating each constraint.
Therefore, we will also need a "get" operation.

```haskell
add :: Constraint -> Tableau -> Tableau
delete :: Integer -> Tableau -> Tableau
get :: Integer -> Tableau -> Constraint
update :: Integer -> Constraint -> Tableau -> Tableau
```

Cassowary's `edit` function _updates_ the constant of a row, and only the constant:

```haskell
edit :: Integer -> Rational -> Tableau -> Tableau
```

> This is the function should compute quickly

### Tableau

A tableau itself is just a matrix of annotated coefficients.

```haskell
data Coeff = Coeff
  { coeffVal    :: Rational
  , coeffName   :: String
  , coeffRow    :: Integer
  , coeffWeight :: Weight ()
  }
```

Because slack variables are generated fresh and free in the rest of the constraint set,
each row's generated slack variables could also be used to index a constraint (as described
in the paper), but for redundancy, we also include the row number. Also, we include the
respective weight caused by each inequality, to impove weighted-sums-better.

Lastly, a tableau is then a 2D matrix of coefficients.

```haskell
type Tableau = Matrix (Mat s) Coeff
```

## TODO

- detail arbitrary variable instantiation required by some constraint operations
- define `weighted-sum-better`
- adopt tests from simplex-basic
