# bioset 0.2.3

  * `set_read`: check `additional_vars` for column names that might get overwritten
  * `canonicalise_units`: fix unit conversion for % w / v (will throw warning)
  
# bioset 0.2.2

  * adapt code to changes coming dplyr 0.7.5 (thanks to krlmlr)

# bioset 0.2.1

  * remove `tidyverse` from "Suggests" as advised by tidyverse authors
  * fix "Dependence on R version '3.4.2' not with patchlevel 0"

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
