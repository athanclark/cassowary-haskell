TODO
====

- Somehow find a compromise between the Map of flat Rationals and a _Trie_ of
  rationals - the trie would be the embodiment of our tuple-based lexicographic ordering.
    - Implement operations discussed in main.pdf

- Prove that

## ReOptimize

- Pushes unrestricted equations to restricted, by the `err_x_+`, `err_x_-` substitution
- Checks new edit constraints, to see if their constant value can just be substituted
    - takes coefficients from objective function during re-assignment? line 5-10 p. 18

## Arithmetic Use Sites

### Division
- `blandRatioPrimal` (`Rational b b`) and `blandRatioDual` (`b b b`) -
  primal is "constant divided by coefficient", and dual is "objective coefficient
  divided by coefficient".
- `flatten` (`b b b` and `Rational b Rational`) - magnify an equation's coefficients
  and constant.

__FUNDEP CONFLICT__: `Rational b b` (weighted bland ratios) vs. `Rational b Rational`
(constant re-magnification)

### Multiplication
- `substitute` (`b b b` and `Rational b b`) - magnify an equation's coefficients and
  constant.

### Substitution
- `substitute` (`b b b` and `Rational b Rational`) - remove a `b` amount from an
  equation's coefficients and constant.


## New Version

- Tableau coefficients may be polymorphic, while the objective function should be
  constrained to Rational
- There should be a class for Simplex and SimplexDual, which may change behavior
  based on the instance of the coefficient - weighted sets, for instance, require
  that we attempt one layer at a time.

## Classes
- HasName - `LinVarName`
- HasStringName - `String` -- lossy info for Error and Slack variables
- HasVariables - `LinVarMap`
- HasCoefficients - `b`
- HasConstant - `Rational`

- SimplexPrimal
  - `nextBasicPrimal`
  - `nextRowPrimal`
    - implicit `blandRatioPrimal`

- SimplexDual
  - `nextRowDual`
  - `nextBasicDual`
    - implicit `blandRatioDual`
