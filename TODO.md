TODO
====

- Make `LinVarMap` parametric in its parameter - to ease making weighted coefficients
    - screws with bland's ratio:
        - Try making the objective function the only site with weighted coefficients,
          and the rest of the constraints in the set weighted per-constraint via a tag.
              - how would substitution affect this?
        - Can we make `Fractional a => Fractional [a]`?
- See if you can make a `dualPivot`, to evade the costly transpose
    - In the bland ratio, choose the `minimum . fmap negate`.

#### ReOptimize

- Pushes unrestricted equations to restricted, by the `err_x_+`, `err_x_-` substitution
- Checks new edit constraints, to see if their constant value can just be substituted
    - takes coefficients from objective function during re-assignment? line 5-10 p. 18
