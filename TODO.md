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
- `blandRatioPrimal` (`Rational b Rational`) and `blandRatioDual` (`b b Rational`) -
  primal is "constant divided by coefficient", and dual is "objective coefficient
  divided by coefficient". __This may be incorrect__.
- `flatten` (`b b b` and `Rational b Rational`) - magnify an equation's coefficients
  and constant.

### Multiplication
- `substitute` (`b b b` and `Rational b b`) - magnify an equation's coefficients and
  constant.

### Substitution
- `substitute` (`b b b` and `Rational b Rational`) - remove a `b` amount from an
  equation's coefficients and constant.
