TODO
====

- Somehow find a compromise between the Map of flat Rationals and a _Trie_ of
  rationals - the trie would be the embodiment of our tuple-based lexicographic ordering.
    - Implement operations discussed in main.pdf

- Prove that 

#### ReOptimize

- Pushes unrestricted equations to restricted, by the `err_x_+`, `err_x_-` substitution
- Checks new edit constraints, to see if their constant value can just be substituted
    - takes coefficients from objective function during re-assignment? line 5-10 p. 18
