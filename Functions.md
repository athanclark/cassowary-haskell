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
augmentedSimplexForm cs f =
  let (cs', c_i) = evalState (makeSlackVars cs) 0
  in restrict cs' c_i f
  where
    makeSlackVars :: ( MonadState Integer m
                     ) => [Constraint]
                       -> m ([Constraint], [Constraint], [Constraint])
    makeSlackVars = foldM go ([],[])
      where
        go (cs c_i) x =
          if x >= 0
          then do s <- get
                  put $ s+1
                  return ( cs
                         , c_i ++ ["s" ++ show s .>=. 0])

    restrict :: [Constraint] -> [Constraint] -> Equality
             -> ([Constraint], [Constraint], [Constraint], Equality)
    restrict cs c_i f =
      foldl' go ([],[],c_i,f) cs
        where
          go (c_u, c_s, c_i, f) c = if isEqual c && isVariable (lhs c) && isVariable (rhs c)
                                      && not $ lhs c `elem` vars (rhs c)
            then let (c_u', c_s', c_i', f') = quadmap (apply (lhs c `to` rhs c)) (c_u, c_s, c_i, f)
                 in (c_u' ++ [c], c_s', c_i', f')
            else (c_u, c_s ++ [c], c_i, f)
```

------

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

becomes

```haskell
-- | Still not in the "functional" way - shouldn't need indicies
simplexOpt :: [Constraint] -> Equality -> Maybe ([Constraint], Equality)
simplexOpt c_s f =
  if all (>= 0) $ coeffs f then (c_s, f)
  else let ratios = mapMaybe (\c -> fmap (\i -> if i < 0
                                                then Just $ - (constant c) / i
                                                else Nothing) c) cs
           mi = go 0 Nothing ratios
             where
               go n macc [] = fst <$> macc
               go n macc (mc:mcs) = case (macc,mc) of
                 (Nothing, Just newc) -> go (n+1) (Just (n, newc)) mcs
                 (Just (idx, oldc), Just newc) -> if newc < oldc
                                                  then go (n+1) (Just (n, newc)) mcs
                                                  else go (n+1) (Just (idx, oldc)) mcs
                 (mold, Nothing) -> go (n+1) mold mcs
           l = go <$> mi
              where
                go i = EVar "x" .+. (constant $ c_s !! i) .+. map negate (params $ c_s !! i)
           subst = "y" `to` l
       in
       -- for each negative parameter var, replace `l` for the `y_j` term in all
       -- except the row chosen from bland's rule - that becomes `y_j == l`.

       (\i -> ( replaceIndex i mapIndex (\idx x -> if idx /= i then apply subst x else x) c_s
              , apply subst f
              )) <$> mi
```

-----

```haskell
reOptimize C_S f =
  foreach stay : v `in` C
    if err_v_+ or err_v_- is basic in row i then c_i := 0 endif
  endfor
  foreach edit : v `in` C
    let alpha and beta be the previous and current edit values for v
    let err_v_+ be y_j
    foreach i {1,...,n}
      c_i := c_i + a_i_j (beta - alpha)
    endfor
  endfor
  repeat
    -- Choose variable xI to become non-basic
    choose I where c_I < 0
    if there is no such I return true endif
    -- Choose variable yJ to become basic
    if for each (j `in` {1,...,m}) (a_I_j <= 0) then
      return false
    endif
    choose J `in` {1,...,m} such that
      d_J / a_I_J = min (j `in` {1,...,m}) {d_j / a_I_j | a_I_j > 0}
    l := (x_I - c_I - sum m (j = 1;j /= J) (a_I_j y_j)) / a_I_J
    replace y_J by l in f
    for each i `in` {1,...,n}
      if i /= I then replace y_J by l in row i endif
    endfor
    replace the Ith row by `y_J = l`
  until false
```

Note: Need to formalize pure matrix form of simplex ^

```haskell
incAddition C_U C_S C_I f s c =
  C_I_old := C_I
  if c is of the form `l >= 0`
    if s is required
      c := l = s' where s' is a new slack variable
      C_I := C_I `union` {s` >= 0}
    else
      c := l = s' - err_c where err_c is a new error variable
      C_I := C_I `union` {s' >= 0, err_c >= 0}
      f := f + s err_c
    endif
  else
    let c be of the form l = 0 where the constant in l is `>= 0`
    if s is not required
      c := l = err_c_+ - err_c_-
      C_I := C_I `union` {err_c_+ >= 0, err_c_- >= 0}
      f := f .+. s err_c_+ .+. s err_c_-
    endif
  endif
  for each x = l in C_U `union` C_S
    replace x in c by l
  endfor
  if exists y `in` vars c - vars C_I
    let c be of the form y = l'
    replace y by l' everywhere in C_U
    C_U := C_U `union` {y = l'}
  elseif exists y `in` vars c - vars C_I_old
          where c is of the form y = l' where the constant in l' is `>= 0`
    C_S := C_S `union` {y = l'}
    replace y by l' in f
    (C_S, f) := simplexOpt C_S f
  else -- c must be a required constraint
    let c be of form l' = 0 where the constant in l' is `>= 0`
    C_S := C_S `union` {a = l'} where a is a new artificial variable
    (C_S, l'') := simplexOpt C_S l'
    if constant part of l'' is not zero then error “unsatisfiable constraints”
    remove variable `a` from C_S (possibly pivoting)
    (C_S, f) := simplexOpt C_S f
  endif
  return (C_U, C_S, C_I, f)
```

```haskell
incDeletion C_U C_S C_I f v =
  foreach stay : c `in` C
    if err_c_+ or err_c_- is basic in row i then c_i := 0 endif
  endfor
  if v is parametric variable y_J
    if y_J `in` vars C_S
      choose I `in` {1,...,n} such that
        -c_I / a_I_J = min (i `in` {1,...,n}) { -c_i / a_i_J | a_i_J < 0}
      remove row I from C_S
    else
      choose arbitrary row y_J = l in C_U
      remove the row from C_U
      replace y_J by l in f
      for each row in C_U `union` CS
        replace y_J by l in row i
      endfor
    endif
  endif
   
  delta := error variables of constraint c for which v is marker
  f := f with coefficients of delta variables set to 0
  (C_S, f) := simplexOpt C_S f
  return (C_U, C_S, C_I, f)
```
