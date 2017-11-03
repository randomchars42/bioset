
<!-- README.md is generated from README.Rmd. Please edit that file -->
bioset
======

-   [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/bioset)](https://cran.r-project.org/package=bioset)
-   latest GitHub-Release: <https://github.com/randomchars42/bioset/releases>

`bioset` is intended to help you working with sets of raw data.

Working in a lab it is not uncommon to have a data set of raw values (because your measuring device spat it out) and you now need to somehow transform and organise the data so that you can work with it.

Installation
------------

A stable version of `bioset` is available on CRAN: <https://cran.r-project.org/package=bioset>

So all you need to do is:

``` r
install.packages("bioset")
```

You can find the latest additions and changes on GitHub. To spare CRAN administrators' time it is requested of all package authors not to submit changes too frequently.

Consequently, I will make new features available on GitHub first. Packages I have not yet submitted to CRAN will be labelled `vX.Y.Z-pre.N` and appear under: <https://github.com/randomchars42/bioset/releases>.

To install those packages you can use [`githubinstall`](https://cran.r-project.org/package=githubinstall)

``` r
# install.packages("githubinstall")
gh_install_packages("bioset", ref = "vX.Y.Z-pre.N")
```

You can install the very latest changes in `bioset`-master from github with:

``` r
# install.packages("devtools")
devtools::install_github("randomchars42/bioset")
```

<!--`bioset` is available on CRAN (<https://CRAN.R-project.org/package=bioset>)-->
Why? What bioset can do for you
-------------------------------

`bioset` lets you:

-   import raw data organised in matrices, e.g. measured values of a 8 x 12 (96-well) bio-assay plate
-   calculate concentrations using samples with known concentrations (calibrators) in your dataset
-   calculate means and variability for duplicates / triplicates / ...
-   convert your concentrations to (more or less) arbitrary units of concentration

Data import
-----------

Suppose you have an `ods` / `xls(x)` file with raw values obtained from a measurement like this:

|     |    1|    2|    3|    4|    5|    6|
|-----|----:|----:|----:|----:|----:|----:|
| A   |  102|  107|  156|  145|  360|  342|
| B   |  198|  203|  101|  121|  231|  226|
| C   |  296|  291|  276|  283|  430|  413|
| D   |  430|  386|  325|  298|  110|  119|

Save them as `set_1.csv`- thats like an `ods` / `xls(x)` file but its basically a text file with the values separated by commas. In the current versions of LibreOffice / OpenOffice / Microsoft office theres an option "Save as" &gt; "csv".

Load the package.

``` r
library("bioset")
```

Then you can use `set_read()` to get all values with their position as name in a nice tibble:

``` r
set_read()
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

`set_read()` automagically reads `set_1.csv` in your current directory. If you have more than one set use `set_read(num = 2)` to read set 2, etc.

If your files are called `plate_1.csv`, `plate_2.csv`, ..., (`run_1.csv`, `run_1.csv`) you can set `file_name = "plate_#NUM#.csv"` (`run_#NUM#.csv`, ...).

If your files are stored in `./files/` tell `set_read()` where to look via `path = "./files/"`.

Naming the values
-----------------

Before feeding your samples into your measuring device you most likely drafted some sort of plan which position corresponds to which sample (didn't you?).

|     | 1    | 2    | 3   | 4   | 5   | 6   |
|-----|:-----|:-----|:----|:----|:----|:----|
| A   | CAL1 | CAL1 | A   | A   | B   | B   |
| B   | CAL2 | CAL2 | C   | C   | D   | D   |
| C   | CAL3 | CAL3 | E   | E   | F   | F   |
| D   | CAL4 | CAL4 | G   | G   | H   | H   |

So you had some calibrators (1-4) and samples A, B, C, D, E, F, G, H, each in duplicates.

To easily set the names for your samples just copy the names into your `set_1.csv`:

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

Tell `set_read()` your data contains the names and which column should hold those names by setting `additional_vars = c("name")`.

``` r
set_read(
  additional_vars = c("name")
)
```

This will get you:

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

Encoding additional properties
------------------------------

Suppose samples A, B, C, D were taken at day 1 and E, F, G, H were taken from the same rats / individuals / patients on day 2.

It would be more elegant to encode that into the data:

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

Calculating concentrations
--------------------------

Propably, your measuring device only gave you raw values (extinction rates / relative light units / ...). You know the concentrations of CAL1, CAL2, CAL3 and CAL4. Conveniently, the concentrations follow a linear relationship. To get the concentrations for the rest of the samples you need to interpolate between those calibrators.

`set_calc_concentrations()` does exactly this for you:

``` r
set_calc_concentrations(
  data,
  cal_names = c("CAL1", "CAL2", "CAL3", "CAL4"),
  cal_values = c(1, 2, 3, 4) # ng / ml
)
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

Duplicates / Triplicates / ...
------------------------------

So samples were measured in duplicates. For our further research you might want to use the mean and perhaps exclude samples with too much spread in their values.

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

|  set| position | sample\_id | name | day |  value|  real|       conc|   recovery|  value\_n|  value\_mean|  value\_sd|  value\_cv|  conc\_n|  conc\_mean|   conc\_sd|   conc\_cv|
|----:|:---------|:-----------|:-----|:----|------:|-----:|----------:|----------:|---------:|------------:|----------:|----------:|--------:|-----------:|----------:|----------:|
|    1| A1       | CAL1       | CAL1 | NA  |    102|     1|  1.0089686|  1.0089686|         2|        104.5|   3.535534|  0.0338329|        2|    1.033881|  0.0352320|  0.0340774|
|    1| B1       | CAL2       | CAL2 | NA  |    198|     2|  1.9656203|  0.9828102|         2|        200.5|   3.535534|  0.0176336|        2|    1.990533|  0.0352320|  0.0176998|
|    1| C1       | CAL3       | CAL3 | NA  |    296|     3|  2.9422023|  0.9807341|         2|        293.5|   3.535534|  0.0120461|        2|    2.917289|  0.0352320|  0.0120770|
|    1| D1       | CAL4       | CAL4 | NA  |    430|     4|  4.2775286|  1.0693822|         2|        408.0|  31.112698|  0.0762566|        2|    4.058296|  0.3100418|  0.0763970|
|    1| A2       | CAL1       | CAL1 | NA  |    107|     1|  1.0587942|  1.0587942|         2|        104.5|   3.535534|  0.0338329|        2|    1.033881|  0.0352320|  0.0340774|
|    1| B2       | CAL2       | CAL2 | NA  |    203|     2|  2.0154459|  1.0077230|         2|        200.5|   3.535534|  0.0176336|        2|    1.990533|  0.0352320|  0.0176998|
|    1| C2       | CAL3       | CAL3 | NA  |    291|     3|  2.8923767|  0.9641256|         2|        293.5|   3.535534|  0.0120461|        2|    2.917289|  0.0352320|  0.0120770|
|    1| D2       | CAL4       | CAL4 | NA  |    386|     4|  3.8390633|  0.9597658|         2|        408.0|  31.112698|  0.0762566|        2|    4.058296|  0.3100418|  0.0763970|
|    1| A3       | A\_1       | A    | 1   |    156|    NA|  1.5470852|         NA|         2|        150.5|   7.778175|  0.0516822|        2|    1.492277|  0.0775105|  0.0519411|
|    1| B3       | C\_1       | C    | 1   |    101|    NA|  0.9990035|         NA|         2|        111.0|  14.142136|  0.1274066|        2|    1.098655|  0.1409281|  0.1282733|
|    1| C3       | A\_2       | A    | 2   |    276|    NA|  2.7428999|         NA|         2|        279.5|   4.949747|  0.0177093|        2|    2.777778|  0.0493248|  0.0177569|
|    1| D3       | C\_2       | C    | 2   |    325|    NA|  3.2311908|         NA|         2|        311.5|  19.091883|  0.0612902|        2|    3.096662|  0.1902529|  0.0614381|
|    1| A4       | A\_1       | A    | 1   |    145|    NA|  1.4374689|         NA|         2|        150.5|   7.778175|  0.0516822|        2|    1.492277|  0.0775105|  0.0519411|
|    1| B4       | C\_1       | C    | 1   |    121|    NA|  1.1983059|         NA|         2|        111.0|  14.142136|  0.1274066|        2|    1.098655|  0.1409281|  0.1282733|
|    1| C4       | A\_2       | A    | 2   |    283|    NA|  2.8126557|         NA|         2|        279.5|   4.949747|  0.0177093|        2|    2.777778|  0.0493248|  0.0177569|
|    1| D4       | C\_2       | C    | 2   |    298|    NA|  2.9621325|         NA|         2|        311.5|  19.091883|  0.0612902|        2|    3.096662|  0.1902529|  0.0614381|
|    1| A5       | B\_1       | B    | 1   |    360|    NA|  3.5799701|         NA|         2|        351.0|  12.727922|  0.0362619|        2|    3.490284|  0.1268353|  0.0363395|
|    1| B5       | D\_1       | D    | 1   |    231|    NA|  2.2944694|         NA|         2|        228.5|   3.535534|  0.0154728|        2|    2.269557|  0.0352320|  0.0155237|
|    1| C5       | B\_2       | B    | 2   |    430|    NA|  4.2775286|         NA|         2|        421.5|  12.020815|  0.0285191|        2|    4.192825|  0.1197889|  0.0285700|
|    1| D5       | D\_2       | D    | 2   |    110|    NA|  1.0886896|         NA|         2|        114.5|   6.363961|  0.0555804|        2|    1.133533|  0.0634176|  0.0559469|
|    1| A6       | B\_1       | B    | 1   |    342|    NA|  3.4005979|         NA|         2|        351.0|  12.727922|  0.0362619|        2|    3.490284|  0.1268353|  0.0363395|
|    1| B6       | D\_1       | D    | 1   |    226|    NA|  2.2446437|         NA|         2|        228.5|   3.535534|  0.0154728|        2|    2.269557|  0.0352320|  0.0155237|
|    1| C6       | B\_2       | B    | 2   |    413|    NA|  4.1081216|         NA|         2|        421.5|  12.020815|  0.0285191|        2|    4.192825|  0.1197889|  0.0285700|
|    1| D6       | D\_2       | D    | 2   |    119|    NA|  1.1783757|         NA|         2|        114.5|   6.363961|  0.0555804|        2|    1.133533|  0.0634176|  0.0559469|

The short way
-------------

If you need to read and transform multiple sets `sets_read` can do that for you.

It takes basically the same arguments as `set_read`, `set_calc_concentrations` and `set_calc_variability` combined and combines their functionality. The principal difference is, that `sets_read` takes `sets` - the number of sets to process.

It returns a list and may (`write_data = TRUE`) create two files in your current directory: `data_all.csv` and `data_samples.csv` with the processed data.

`sets_read()`'s list holds the following items:

-   `$all`: here you will find all the data , including calibrators, duplicates, ... (saved in `data_all.csv` if `write_data = TRUE`)
-   `$samples`: only one row per distinct sample here - no calibrators, no duplicates -&gt; most often you will work with this data (saved in `data_samples.csv` if `write_data = TRUE`)
-   `$set1`: a list
    -   `$plot`: a plot showing you the function used to calculate the concentrations for this set. The points represent the calibrators.
    -   `$model`: the model as returned by `model_func`
-   (`$set2` - `$setN`): the same information for every set you have

Take a look at the data

``` r
# now you may run it :)
result_list <- sets_read(
  sets = 1,
  sep = ",",
  additional_vars = c("name", "day"),
  cal_names = c("CAL1", "CAL2", "CAL3", "CAL4"),
  cal_values = c(1, 2, 3, 4) # ng / ml
)
```

``` r
result_list$all
```

|  set| position | sample\_id | name | day |  value|  real|   recovery|    n|  raw|  raw\_mean|    raw\_sd|    raw\_cv|  concentration|  concentration\_sd|  concentration\_cv|
|----:|:---------|:-----------|:-----|:----|------:|-----:|----------:|----:|----:|----------:|----------:|----------:|--------------:|------------------:|------------------:|
|    1| A1       | CAL1       | CAL1 | NA  |    102|     1|  1.0089686|    2|  102|      104.5|   3.535534|  0.0338329|       1.033881|          0.0352320|          0.0340774|
|    1| B1       | CAL2       | CAL2 | NA  |    198|     2|  0.9828102|    2|  198|      200.5|   3.535534|  0.0176336|       1.990533|          0.0352320|          0.0176998|
|    1| C1       | CAL3       | CAL3 | NA  |    296|     3|  0.9807341|    2|  296|      293.5|   3.535534|  0.0120461|       2.917289|          0.0352320|          0.0120770|
|    1| D1       | CAL4       | CAL4 | NA  |    430|     4|  1.0693822|    2|  430|      408.0|  31.112698|  0.0762566|       4.058296|          0.3100418|          0.0763970|
|    1| A2       | CAL1       | CAL1 | NA  |    107|     1|  1.0587942|    2|  107|      104.5|   3.535534|  0.0338329|       1.033881|          0.0352320|          0.0340774|
|    1| B2       | CAL2       | CAL2 | NA  |    203|     2|  1.0077230|    2|  203|      200.5|   3.535534|  0.0176336|       1.990533|          0.0352320|          0.0176998|
|    1| C2       | CAL3       | CAL3 | NA  |    291|     3|  0.9641256|    2|  291|      293.5|   3.535534|  0.0120461|       2.917289|          0.0352320|          0.0120770|
|    1| D2       | CAL4       | CAL4 | NA  |    386|     4|  0.9597658|    2|  386|      408.0|  31.112698|  0.0762566|       4.058296|          0.3100418|          0.0763970|
|    1| A3       | A\_1       | A    | 1   |    156|    NA|         NA|    2|  156|      150.5|   7.778175|  0.0516822|       1.492277|          0.0775105|          0.0519411|
|    1| B3       | C\_1       | C    | 1   |    101|    NA|         NA|    2|  101|      111.0|  14.142136|  0.1274066|       1.098655|          0.1409281|          0.1282733|
|    1| C3       | A\_2       | A    | 2   |    276|    NA|         NA|    2|  276|      279.5|   4.949747|  0.0177093|       2.777778|          0.0493248|          0.0177569|
|    1| D3       | C\_2       | C    | 2   |    325|    NA|         NA|    2|  325|      311.5|  19.091883|  0.0612902|       3.096662|          0.1902529|          0.0614381|
|    1| A4       | A\_1       | A    | 1   |    145|    NA|         NA|    2|  145|      150.5|   7.778175|  0.0516822|       1.492277|          0.0775105|          0.0519411|
|    1| B4       | C\_1       | C    | 1   |    121|    NA|         NA|    2|  121|      111.0|  14.142136|  0.1274066|       1.098655|          0.1409281|          0.1282733|
|    1| C4       | A\_2       | A    | 2   |    283|    NA|         NA|    2|  283|      279.5|   4.949747|  0.0177093|       2.777778|          0.0493248|          0.0177569|
|    1| D4       | C\_2       | C    | 2   |    298|    NA|         NA|    2|  298|      311.5|  19.091883|  0.0612902|       3.096662|          0.1902529|          0.0614381|
|    1| A5       | B\_1       | B    | 1   |    360|    NA|         NA|    2|  360|      351.0|  12.727922|  0.0362619|       3.490284|          0.1268353|          0.0363395|
|    1| B5       | D\_1       | D    | 1   |    231|    NA|         NA|    2|  231|      228.5|   3.535534|  0.0154728|       2.269557|          0.0352320|          0.0155237|
|    1| C5       | B\_2       | B    | 2   |    430|    NA|         NA|    2|  430|      421.5|  12.020815|  0.0285191|       4.192825|          0.1197889|          0.0285700|
|    1| D5       | D\_2       | D    | 2   |    110|    NA|         NA|    2|  110|      114.5|   6.363961|  0.0555804|       1.133533|          0.0634176|          0.0559469|
|    1| A6       | B\_1       | B    | 1   |    342|    NA|         NA|    2|  342|      351.0|  12.727922|  0.0362619|       3.490284|          0.1268353|          0.0363395|
|    1| B6       | D\_1       | D    | 1   |    226|    NA|         NA|    2|  226|      228.5|   3.535534|  0.0154728|       2.269557|          0.0352320|          0.0155237|
|    1| C6       | B\_2       | B    | 2   |    413|    NA|         NA|    2|  413|      421.5|  12.020815|  0.0285191|       4.192825|          0.1197889|          0.0285700|
|    1| D6       | D\_2       | D    | 2   |    119|    NA|         NA|    2|  119|      114.5|   6.363961|  0.0555804|       1.133533|          0.0634176|          0.0559469|

``` r
result_list$samples
```

| position | sample\_id | name | day |  plate|    n|    raw|    raw\_sd|    raw\_cv|  concentration|  concentration\_sd|  concentration\_cv|
|:---------|:-----------|:-----|:----|------:|----:|------:|----------:|----------:|--------------:|------------------:|------------------:|
| A3       | A\_1       | A    | 1   |      1|    2|  150.5|   7.778175|  0.0516822|       1.492277|          0.0775105|          0.0519411|
| B3       | C\_1       | C    | 1   |      1|    2|  111.0|  14.142136|  0.1274066|       1.098655|          0.1409281|          0.1282733|
| C3       | A\_2       | A    | 2   |      1|    2|  279.5|   4.949747|  0.0177093|       2.777778|          0.0493248|          0.0177569|
| D3       | C\_2       | C    | 2   |      1|    2|  311.5|  19.091883|  0.0612902|       3.096662|          0.1902529|          0.0614381|
| A5       | B\_1       | B    | 1   |      1|    2|  351.0|  12.727922|  0.0362619|       3.490284|          0.1268353|          0.0363395|
| B5       | D\_1       | D    | 1   |      1|    2|  228.5|   3.535534|  0.0154728|       2.269557|          0.0352320|          0.0155237|
| C5       | B\_2       | B    | 2   |      1|    2|  421.5|  12.020815|  0.0285191|       4.192825|          0.1197889|          0.0285700|
| D5       | D\_2       | D    | 2   |      1|    2|  114.5|   6.363961|  0.0555804|       1.133533|          0.0634176|          0.0559469|

``` r
result_list$set1$plot
```
