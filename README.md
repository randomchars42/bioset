
<!-- README.md is generated from README.Rmd. Please edit that file -->
bioset
======

bioset is intended to save you from some dull tasks when dealing with raw data obtained e.g. from a measuring device.

Installation
------------

You can install bioset from github with:

``` r
# install.packages("devtools")
devtools::install_github("randomchars42/bioset")
```

Why? What bioset can do for you
-------------------------------

bioset lets you:

-   import raw data organised in matrices, e.g. measured values of a 9 x 12 bio-assay plate
-   calculate concentrations using samples with known concentrations (calibrators) in your dataset
-   calculate means and variability for duplicates / triplicates / ...
-   convert your concentrations to (more or less) arbitrary units of concentration

Example
-------

If you have a data set of raw values (because your measuring device spat it out) and need to somehow organise the data this package might help you.

### Data import

Suppose you have an ods / xls(x) file with raw values obtained from a measurement like this:

``` r
data <-
  utils::read.csv(
    system.file("extdata", "values.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:4]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))
```

|     |    1|    2|    3|    4|    5|    6|
|-----|----:|----:|----:|----:|----:|----:|
| A   |  102|  107|  156|  145|  360|  342|
| B   |  198|  203|  101|  121|  231|  226|
| C   |  296|  291|  276|  283|  430|  413|
| D   |  430|  386|  325|  298|  110|  119|

Save them as `set_1.csv` (like an ods / xls(x) file but much more basic).

Then you can use `set_read()` to get all values with their position as name in a nice tibble:

``` r
set_read()
```

``` r
data <- bioset::set_read(
  file_name = system.file("extdata", "values.csv", package = "bioset")
)
knitr::kable(data)
```

|  set| position | sample\_id | name |  value|
|----:|:---------|:-----------|:-----|------:|
|    1| A1       | A1         | A1   |    102|
|    1| B1       | B1         | B1   |    198|
|    1| C1       | C1         | C1   |    296|
|    1| D1       | D1         | D1   |    430|
|    1| A2       | A2         | A2   |    107|
|    1| B2       | B2         | B2   |    203|
|    1| C2       | C2         | C2   |    291|
|    1| D2       | D2         | D2   |    386|
|    1| A3       | A3         | A3   |    156|
|    1| B3       | B3         | B3   |    101|
|    1| C3       | C3         | C3   |    276|
|    1| D3       | D3         | D3   |    325|
|    1| A4       | A4         | A4   |    145|
|    1| B4       | B4         | B4   |    121|
|    1| C4       | C4         | C4   |    283|
|    1| D4       | D4         | D4   |    298|
|    1| A5       | A5         | A5   |    360|
|    1| B5       | B5         | B5   |    231|
|    1| C5       | C5         | C5   |    430|
|    1| D5       | D5         | D5   |    110|
|    1| A6       | A6         | A6   |    342|
|    1| B6       | B6         | B6   |    226|
|    1| C6       | C6         | C6   |    413|
|    1| D6       | D6         | D6   |    119|

`set_read()` automagically reads `set_1.csv` in your current directory. If you have more than one set use `set_read(num = 2)` to read set 2, etc. If your files are called `plate_1.csv`, `plate_2.csv`, ..., (`run_1.csv`, `run_1.csv`) you can set `file_name = "plate_#NUM#.csv"` (`run_#NUM#.csv`, ...). If your files are stored in `./files/` tell `set_read()` where to look via `path = "./files/"`.

### Naming the values

Before feeding your samples into your measuring device you most likely drafted some sort of plan which position corresponds to which sample (didn't you?).

``` r
data <-
  utils::read.csv(
    system.file("extdata", "names.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:4]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))
```

|     | 1    | 2    | 3   | 4   | 5   | 6   |
|-----|:-----|:-----|:----|:----|:----|:----|
| A   | CAL1 | CAL1 | A   | A   | B   | B   |
| B   | CAL2 | CAL2 | C   | C   | D   | D   |
| C   | CAL3 | CAL3 | E   | E   | F   | F   |
| D   | CAL4 | CAL4 | G   | G   | H   | H   |

So you had some calibrators (1-4) and samples A, B, C, D, E, F, G, H, each in duplicates.

To easily set the names for your samples just copy the names into your `set_1.csv`:

``` r
data <-
  utils::read.csv(
    system.file("extdata", "values_names.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:8]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))
```

|     | 1    | 2    | 3   | 4   | 5   | 6   |
|-----|:-----|:-----|:----|:----|:----|:----|
| A   | 102  | 107  | 156 | 145 | 360 | 342 |
| B   | 198  | 203  | 101 | 121 | 231 | 226 |
| C   | 296  | 291  | 276 | 283 | 430 | 413 |
| D   | 430  | 386  | 325 | 298 | 110 | 119 |
| E   | CAL1 | CAL1 | A   | A   | B   | B   |
| F   | CAL2 | CAL2 | C   | C   | D   | D   |
| G   | CAL3 | CAL3 | E   | E   | F   | F   |
| H   | CAL4 | CAL4 | G   | G   | H   | H   |

Tell `set_read()` your data contains the names and which column should hold those names by setting `additional_vars = c("name")`. This will get you:

``` r
set_read(
  additional_vars = c("name")
)
```

``` r
data <- bioset::set_read(
  file_name = system.file("extdata", "values_names.csv", package = "bioset"),
  additional_vars = c("name")
)
knitr::kable(data)
```

|  set| position | sample\_id | name |  value|
|----:|:---------|:-----------|:-----|------:|
|    1| A1       | CAL1       | CAL1 |    102|
|    1| B1       | CAL2       | CAL2 |    198|
|    1| C1       | CAL3       | CAL3 |    296|
|    1| D1       | CAL4       | CAL4 |    430|
|    1| A2       | CAL1       | CAL1 |    107|
|    1| B2       | CAL2       | CAL2 |    203|
|    1| C2       | CAL3       | CAL3 |    291|
|    1| D2       | CAL4       | CAL4 |    386|
|    1| A3       | A          | A    |    156|
|    1| B3       | C          | C    |    101|
|    1| C3       | E          | E    |    276|
|    1| D3       | G          | G    |    325|
|    1| A4       | A          | A    |    145|
|    1| B4       | C          | C    |    121|
|    1| C4       | E          | E    |    283|
|    1| D4       | G          | G    |    298|
|    1| A5       | B          | B    |    360|
|    1| B5       | D          | D    |    231|
|    1| C5       | F          | F    |    430|
|    1| D5       | H          | H    |    110|
|    1| A6       | B          | B    |    342|
|    1| B6       | D          | D    |    226|
|    1| C6       | F          | F    |    413|
|    1| D6       | H          | H    |    119|

### Encoding additional properties

Suppose samples A, B, C, D were taken at day 1 and E, F, G, H were taken from the same rats / individuals / patients / students on day 2.

It would be more elegant to encode that into the data:

``` r
data <-
  utils::read.csv(
    system.file("extdata", "values_names_properties.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:8]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))
```

|     | 1    | 2    | 3    | 4    | 5    | 6    |
|-----|:-----|:-----|:-----|:-----|:-----|:-----|
| A   | 102  | 107  | 156  | 145  | 360  | 342  |
| B   | 198  | 203  | 101  | 121  | 231  | 226  |
| C   | 296  | 291  | 276  | 283  | 430  | 413  |
| D   | 430  | 386  | 325  | 298  | 110  | 119  |
| E   | CAL1 | CAL1 | A\_1 | A\_1 | B\_1 | B\_1 |
| F   | CAL2 | CAL2 | C\_1 | C\_1 | D\_1 | D\_1 |
| G   | CAL3 | CAL3 | A\_2 | A\_2 | B\_2 | B\_2 |
| H   | CAL4 | CAL4 | C\_2 | C\_2 | D\_2 | D\_2 |

Now, tell `set_read()` your data contains the names and day by setting `additional_vars = c("name", "day")`. This will get you:

``` r
set_read(
  additional_vars = c("name", "day")
)
```

``` r
data <- bioset::set_read(
  file_name = system.file("extdata", "values_names_properties.csv", package = "bioset"),
  additional_vars = c("name", "day")
)

knitr::kable(data)
```

|  set| position | sample\_id | name | day |  value|
|----:|:---------|:-----------|:-----|:----|------:|
|    1| A1       | CAL1       | CAL1 | NA  |    102|
|    1| B1       | CAL2       | CAL2 | NA  |    198|
|    1| C1       | CAL3       | CAL3 | NA  |    296|
|    1| D1       | CAL4       | CAL4 | NA  |    430|
|    1| A2       | CAL1       | CAL1 | NA  |    107|
|    1| B2       | CAL2       | CAL2 | NA  |    203|
|    1| C2       | CAL3       | CAL3 | NA  |    291|
|    1| D2       | CAL4       | CAL4 | NA  |    386|
|    1| A3       | A\_1       | A    | 1   |    156|
|    1| B3       | C\_1       | C    | 1   |    101|
|    1| C3       | A\_2       | A    | 2   |    276|
|    1| D3       | C\_2       | C    | 2   |    325|
|    1| A4       | A\_1       | A    | 1   |    145|
|    1| B4       | C\_1       | C    | 1   |    121|
|    1| C4       | A\_2       | A    | 2   |    283|
|    1| D4       | C\_2       | C    | 2   |    298|
|    1| A5       | B\_1       | B    | 1   |    360|
|    1| B5       | D\_1       | D    | 1   |    231|
|    1| C5       | B\_2       | B    | 2   |    430|
|    1| D5       | D\_2       | D    | 2   |    110|
|    1| A6       | B\_1       | B    | 1   |    342|
|    1| B6       | D\_1       | D    | 1   |    226|
|    1| C6       | B\_2       | B    | 2   |    413|
|    1| D6       | D\_2       | D    | 2   |    119|

### Calculating concentrations

Your measuring device only gave you raw values (extinction rates / relative light units / ...). You know the concentrations of CAL1, CAL2, CAL3 and CAL4. Conveniently, the concentrations follow a linear relationship. To get the concentrations for the rest of the samples you need to interpolate between those calibrators.

`set_calc_concentrations()` does exactly this for you:

``` r
set_calc_concentrations(
  data,
  cal_names = c("CAL1", "CAL2", "CAL3", "CAL4"),
  cal_values = c(1, 2, 3, 4) # ng / ml
)
```

``` r
data <- bioset::set_calc_concentrations(
  data,
  cal_names = c("CAL1", "CAL2", "CAL3", "CAL4"),
  cal_values = c(1, 2, 3, 4) # ng / ml
)

knitr::kable(data)
```

|  set| position | sample\_id | name | day |  value|  real|       conc|   recovery|
|----:|:---------|:-----------|:-----|:----|------:|-----:|----------:|----------:|
|    1| A1       | CAL1       | CAL1 | NA  |    102|     1|  1.0089686|  1.0089686|
|    1| B1       | CAL2       | CAL2 | NA  |    198|     2|  1.9656203|  0.9828102|
|    1| C1       | CAL3       | CAL3 | NA  |    296|     3|  2.9422023|  0.9807341|
|    1| D1       | CAL4       | CAL4 | NA  |    430|     4|  4.2775286|  1.0693822|
|    1| A2       | CAL1       | CAL1 | NA  |    107|     1|  1.0587942|  1.0587942|
|    1| B2       | CAL2       | CAL2 | NA  |    203|     2|  2.0154459|  1.0077230|
|    1| C2       | CAL3       | CAL3 | NA  |    291|     3|  2.8923767|  0.9641256|
|    1| D2       | CAL4       | CAL4 | NA  |    386|     4|  3.8390633|  0.9597658|
|    1| A3       | A\_1       | A    | 1   |    156|    NA|  1.5470852|         NA|
|    1| B3       | C\_1       | C    | 1   |    101|    NA|  0.9990035|         NA|
|    1| C3       | A\_2       | A    | 2   |    276|    NA|  2.7428999|         NA|
|    1| D3       | C\_2       | C    | 2   |    325|    NA|  3.2311908|         NA|
|    1| A4       | A\_1       | A    | 1   |    145|    NA|  1.4374689|         NA|
|    1| B4       | C\_1       | C    | 1   |    121|    NA|  1.1983059|         NA|
|    1| C4       | A\_2       | A    | 2   |    283|    NA|  2.8126557|         NA|
|    1| D4       | C\_2       | C    | 2   |    298|    NA|  2.9621325|         NA|
|    1| A5       | B\_1       | B    | 1   |    360|    NA|  3.5799701|         NA|
|    1| B5       | D\_1       | D    | 1   |    231|    NA|  2.2944694|         NA|
|    1| C5       | B\_2       | B    | 2   |    430|    NA|  4.2775286|         NA|
|    1| D5       | D\_2       | D    | 2   |    110|    NA|  1.0886896|         NA|
|    1| A6       | B\_1       | B    | 1   |    342|    NA|  3.4005979|         NA|
|    1| B6       | D\_1       | D    | 1   |    226|    NA|  2.2446437|         NA|
|    1| C6       | B\_2       | B    | 2   |    413|    NA|  4.1081216|         NA|
|    1| D6       | D\_2       | D    | 2   |    119|    NA|  1.1783757|         NA|

Your calibrators are not so linear? Perhaps after a ln-ln transformation? You can use: `model_func = fit_lnln` and `interpolate_func = interpolate_lnln`. Basicallly, you can use any function as `model_function` that returns a model which is understood by your `interpolate-func`.

### Duplicates / Triplicates / ...

So samples were measured in duplicates. For our further research we might want to use the mean and perhaps exclude samples with too much spread in their values.

`set_calc_variability()` to the rescue.

``` r
data <- set_calc_variability(
  data = data,
  ids = sample_id,
  value,
  conc
)
```

This will give you the mean and coefficient of variation (as well as n of the samples and the standard deviation) for the columns `value` and `conc`. It will use `sample_id` to determine which rows belong to the same sample.

``` r
data <- bioset::set_calc_variability(
  data = data,
  ids = sample_id,
  value,
  conc
)

knitr::kable(data)
```
