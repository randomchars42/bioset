# bioset 0.2.0

  * add `sets_read`
  * add option to import csv with "," as decimal point
  * `calc_conc` / `calc_prefix` / `convert_conc` / `convert_prefix`:
      * add more rigorous checks on input
      * will throw an error if the unit is not recognised
      * be more permissive with whitespace in input
      * add more tests
      * permit more metric prefixes
      * add more examples
  * fix error in `factor_conc`:
      * `factor_conc` did not stop if `molar_mass` / `density_solute` /
        `densitiy_solution` were required but missing
      * add unit tests

# bioset 0.1.0

  * initial release
