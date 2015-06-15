Functions
=========

These are the functions defined in the paper:

```haskell
augmentedSimplexForm C f =
  C_I := ∅; C_U := ∅;
  for each inequality l >= 0 in C -- generate slack vars
    C := C - {l >= 0}
    let s be a new slack variable
    C := C ∪ {l = s}
    C_I := C_I ∪ {s >= 0}
  endfor
  while exists variable y in (vars C - vars C_I) -- generate unrestricted set
    let y = l ∈ C where y ∉ vars l
    C := C - {c}
    replace y by l in (C_U ∪ C) and in f
    C_U := C_U ∪ {y = l}
  endwhile
  C_S := C -- restricted is now the difference
  return (C_U, C_S, C_I, f)
```

turns into

```haskell
-- | Splits a set of constraints into their restricted, unrestricted, and
-- slack variables
augmentedSimplexForm :: ([Constraint], Equality)
                     -> ([Constraint], [Constraint], [Constraint], Equality)
augmentedSimplexForm C f =
  let 
```

```haskell
-- | simplex optimization for constraint set `C_S` with objective function `f`
-- in basic feasible form
simplexOpt C_S f =
  repeat
    -- Choose variable y_j to become basic
    if for each j ∈ {1,...,m} d_j >= 0 then
      return (C_S, f) -- an optimal solution has been found
    endif
    choose J ∈ {1,...,m} such that d_J < 0
    -- Choose variable x_I to become non-basic
    choose I ∈ {1,...,n} such that
      -c_I / a_I_J = min (i ∈ {1,...,n}) { -c_i / a_i_J | a_i_J < 0 }
    l := (x_I - c_I - sum m [j==1, j /= J] (a_I_j y_j)) / a_I_J
    replace y_J by l in f
    for each i ∈ {1,...,n}
      if i /= I then replace y_J by l in row i endif
    endfor
    replace the Ith row by y_J = l
  endrepeat
```

```haskell
reOptimize C_S f
  foreach stay : v ∈ C
    if Æ v + or Æv is basic in row i then c i := 0 endif
  foreach endfor
   edit : v 2
   C
  let  and  be the previous and current edit values forlet Æv + be yj
  foreach i
   2 f1
  ;:::;n
  g
  ci := ci + aij (
   
  )
  endfor
  endfor
  repeat
  % Choose variable xI to become non-basic
  choose I where c I <
   0
  if there is no such I
  return true
  endif
  % Choose variable yJ to become basic
  if for each j
   2 f1
  ; : : : ; mg
   aI j
    0
   then
  return false
  endif
   choose J
   2 f1
  ; : : : ; mg
   such that
  dJ =aI J = min
   P j m 2f1;:::;mg fdj =aI j j aIj > 0g
  l := (xI cI
   j=1;j 6=J
   aIj yj
   )=aIJ
  replace y J by l in f
  for each i 2 f1; : : : ; ng
  if i 6= I then replace y J by l in row i endif
  endfor
   replace the Ith row by yJ
   = l
  until false
  v
```
