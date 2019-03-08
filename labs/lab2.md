Data Wrangling: How to Clean your Data
================

We will be using the `tidyverse` in this lab. So let’s start by loading
it.

``` r
library(tidyverse)
```

Suppose your lab conducts an experiment in which four different
fragments of chromosome 21 were integrated into mice. The four parts are
denoted with *141G6*, *152F7*, *230E8* and *285E6*. The mice were bred
resulting in dozens of transgenic mice. The DNA fragment is not always
inherted so some mice have the extra copy and others don’t. Furthermore,
suppose the data was stored as two datasets: **weight.csv** and
**blood-pressure.csv**. The goal of this lecture is to clean and merge
the datasets in order to end up with this:

| DNA | line       | tg | sex | age | weight |    bp | cage |
| --: | :--------- | -: | --: | --: | -----: | ----: | ---: |
|   3 | \#50-69-1  |  1 |   1 | 113 |   31.6 | 123.4 |    1 |
|   3 | \#50-69-2  |  1 |   1 | 113 |   31.2 | 125.3 |    1 |
|   3 | \#50-69-3  |  1 |   1 | 113 |   28.6 | 122.0 |    1 |
|   3 | \#50-69-4  |  0 |   1 | 113 |   30.1 | 126.0 |    1 |
|   3 | \#50-69-11 |  0 |   1 | 121 |   31.3 | 128.6 |    2 |
|   3 | \#50-69-12 |  0 |   1 | 121 |   36.4 | 125.5 |    2 |

The columns included in this table are the following:

  - *DNA* dd: Fragment of chromosome 21 integrated in parent mouse
    (1=141G6; 2=152F7; 3=230E8; 4=285E6).
  - *line* : Family line.
  - *tg* : Whether the mouse contains the extra DNA (1) or not (0).
  - *sex* : Sex of mouse (1=male; 0=female).
  - *age* : Age of mouse (in days) at time of weighing.
  - *weight*: Weight of mouse in grams, to the nearest tenth of a gram.
  - *bp* : Blood pressure of the mosue.
  - *cage* : Number of the cage in which the mouse lived

Let’s start by loading the data and looking at it

``` r
library(readxl)
bp     <- read_excel("../data/mouse-raw-data.xlsx", sheet=1)
weight <- read_excel("../data/mouse-raw-data.xlsx", sheet=2) 
```

Now we can look at the data. Here is a sample of **blood-pressure.csv**:

| line.1 | line.2 | line.3 |    bp |
| -----: | -----: | :----- | ----: |
|     12 |     14 | 1      | 118.3 |
|     12 |     14 | 2      | 123.3 |
|     12 |     14 | 3      | 116.3 |
|     12 |     14 | 4      | 125.4 |
|     12 |     14 | 5      | 119.3 |
|     12 |     14 | 6      | 121.9 |

Each row in this dataset correspond to a mouse. The first three columns
describe the family line of the mouse and the last columns corresponds
to a blood pressure measurement. Here is a sample of
**weight.csv**:

| 141G6.line | 141G6.tg | 141G6.sex | 141G6.age | 141G6.weight | 141G6.cage |
| :--------- | -------: | --------: | --------: | -----------: | ---------: |
| \#4-77-117 |        1 |         1 |       119 |         31.2 |          7 |
| \#4-77-118 |        1 |         1 |       119 |         28.4 |          7 |
| \#4-77-119 |        1 |         1 |       119 |         30.8 |          7 |
| \#4-77-120 |        1 |         1 |       119 |         30.3 |          7 |
| \#4-77-121 |        1 |         1 |       119 |         30.5 |          7 |
| \#4-77-122 |        1 |         1 |       119 |         31.4 |          7 |

The column names in this table correspond to the characteristics of mice
at each DNA fragment. Here we show a sample of observations that have
the *141G6* fragment. Try looking at **weight.csv** for yourself and
notice the other columns. We need to clean a lot of data\!

## Wrangling BP dataset

Let’s start with **blood-pressure.csv**. The first thing we want to do
is to create a new variable that is the combination of the first three
columns of the table. We will call this new variable `line`

``` r
bp <- bp %>%
      mutate(line = paste(line.1, line.2, line.3, sep = "-"))
```

| line.1 | line.2 | line.3 |    bp | line    |
| -----: | -----: | :----- | ----: | :------ |
|     12 |     14 | 1      | 118.3 | 12-14-1 |
|     12 |     14 | 2      | 123.3 | 12-14-2 |
|     12 |     14 | 3      | 116.3 | 12-14-3 |
|     12 |     14 | 4      | 125.4 | 12-14-4 |
|     12 |     14 | 5      | 119.3 | 12-14-5 |
|     12 |     14 | 6      | 121.9 | 12-14-6 |

Notice that the `line` data in **weight.csv** has a \# symbol at the
beginning. Let’s add that:

$$

$$

**Question 1**: Why would we want these to be the same?

$$

$$

Now, remove the columns `line.1`, `line.2`, and `line.3` from the data.
**HINT**: Use the function `select`.

$$

$$

## Wrangling Weight dataset

Let’s now move onto **weight.csv**. Recall that the names in this table
correspond to the characteristics of mice at each DNA fragment. You can
see this by printing the column names:

``` r
colnames(weight)
```

    ##  [1] "141G6.line"   "141G6.tg"     "141G6.sex"    "141G6.age"   
    ##  [5] "141G6.weight" "141G6.cage"   "152F7.line"   "152F7.tg"    
    ##  [9] "152F7.sex"    "152F7.age"    "152F7.weight" "152F7.cage"  
    ## [13] "230E8.line"   "230E8.tg"     "230E8.sex"    "230E8.age"   
    ## [17] "230E8.weight" "230E8.cage"   "285E6.line"   "285E6.tg"    
    ## [21] "285E6.sex"    "285E6.age"    "285E6.weight" "285E6.cage"

To start easy, let’s consider only the data regarding the *141G6*
fragment. How do you do this? **Hint**: Use `select` and `contains`

$$

$$

Any ideas on how to proceed? Do we need to do much more?

No\! Notice that all the data regarding this DNA fragment is in that
table. All we need to do know is change the name of the variables and
create a new variable `DNA`. Try doing this by yourself. **Hint**: Use
`rename` and `mutate`:

$$

$$

Now, repeat this procedure for all DNA fragments. **Hint**: Use loops

$$

$$

We are almost done. Recall from above that `DNA` is encoded as:

  - 1 = 141G6
  - 2 = 152F7
  - 3 = 230E8
  - 4 = 285E6

We can use the function `recode` to achieve this:

``` r
weight <- weight %>%
            mutate(DNA = recode(DNA, "141G6"="1", "152F7"="2", "230E8"="3", "285E6"="4"))
```

Finally, the last step is to merge the two datasets. Recall that we want
to use the `line` variable to merge the tables. Give it a try, call the
new table `dat`. **Hint**: Use the `left_join` function

$$

$$

# Exercises

1.  Check the variable types of each column. Does it make sense? Should
    we change the variable type of any of the columns?

$$

$$

2.  Compute the number of mice in each DNA fragment group. Are the
    number of observations the same? **Hint**: Use `group_by` and
    `summarize`

$$

$$

3.  Above we used `weight %>% left_join(bp, by = "line")` to merge the
    two tables. What happens when you run `bp %>% left_join(weight, by =
    "line")`? What about `weight %>% right_join(bp, by = "line")`? Do we
    end up with the same results? **Hint**: What are the row numbers of
    the tables? Use `anyNA` and `na.omit`

$$

$$
