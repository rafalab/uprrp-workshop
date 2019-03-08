Data Wrangling: How to Clean your Data (Solutions)
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

  - *DNA*: Fragment of chromosome 21 integrated in parent mouse
    (1=141G6; 2=152F7; 3=230E8; 4=285E6).
  - *line*: Family line.
  - *tg* - Whether the mouse contains the extra DNA (1) or not (0).
  - *sex*: Sex of mouse (1=male; 0=female).
  - *age*: Age of mouse (in days) at time of weighing.
  - *weight*: Weight of mouse in grams, to the nearest tenth of a gram.
  - *bp*: Blood pressure of the mosue.
  - *cage*: Number of the cage in which the mouse lived

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

``` r
bp <- bp %>%
      mutate(line = paste("#", line, sep = ""))
```

| line.1 | line.2 | line.3 |    bp | line      |
| -----: | -----: | :----- | ----: | :-------- |
|     12 |     14 | 1      | 118.3 | \#12-14-1 |
|     12 |     14 | 2      | 123.3 | \#12-14-2 |
|     12 |     14 | 3      | 116.3 | \#12-14-3 |
|     12 |     14 | 4      | 125.4 | \#12-14-4 |
|     12 |     14 | 5      | 119.3 | \#12-14-5 |
|     12 |     14 | 6      | 121.9 | \#12-14-6 |

**QUESTION**: Why would we want these to be the same? Recall that the
end goal is to merge the two datasets. Furthermore, notice that the only
variable in common between the two datasets is `line`. Therefore, we
will use `line` to merge the two datasets.

Now, remove the columns `line.1`, `line.2`, and `line.3` from the data.
**HINT**: Use the function `select`.

``` r
bp <- bp %>% select(line, bp)
```

| line      |    bp |
| :-------- | ----: |
| \#12-14-1 | 118.3 |
| \#12-14-2 | 123.3 |
| \#12-14-3 | 116.3 |
| \#12-14-4 | 125.4 |
| \#12-14-5 | 119.3 |
| \#12-14-6 | 121.9 |

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

``` r
weight %>% 
  select(contains("141G6"))
```

| 141G6.line | 141G6.tg | 141G6.sex | 141G6.age | 141G6.weight | 141G6.cage |
| :--------- | -------: | --------: | --------: | -----------: | ---------: |
| \#4-77-117 |        1 |         1 |       119 |         31.2 |          7 |
| \#4-77-118 |        1 |         1 |       119 |         28.4 |          7 |
| \#4-77-119 |        1 |         1 |       119 |         30.8 |          7 |
| \#4-77-120 |        1 |         1 |       119 |         30.3 |          7 |
| \#4-77-121 |        1 |         1 |       119 |         30.5 |          7 |
| \#4-77-122 |        1 |         1 |       119 |         31.4 |          7 |

Any ideas on how to proceed? Do we need to do much more?

No\! Notice that all the data regarding this DNA fragment is in that
table. All we need to do know is change the name of the variables and
create a new variable `DNA`. Try doing this by yourself. **Hint**: Use
`rename` and `mutate`:

``` r
weight %>% 
  select(contains("141G6")) %>%
  rename(tg = "141G6.tg",
         sex    = "141G6.sex",
         age    = "141G6.age",
         weight = "141G6.weight",
         cage   = "141G6.cage",
         line   = "141G6.line") %>%
  mutate(DNA = "141G6") 
```

| line       | tg | sex | age | weight | cage | DNA   |
| :--------- | -: | --: | --: | -----: | ---: | :---- |
| \#4-77-117 |  1 |   1 | 119 |   31.2 |    7 | 141G6 |
| \#4-77-118 |  1 |   1 | 119 |   28.4 |    7 | 141G6 |
| \#4-77-119 |  1 |   1 | 119 |   30.8 |    7 | 141G6 |
| \#4-77-120 |  1 |   1 | 119 |   30.3 |    7 | 141G6 |
| \#4-77-121 |  1 |   1 | 119 |   30.5 |    7 | 141G6 |
| \#4-77-122 |  1 |   1 | 119 |   31.4 |    7 | 141G6 |

Now, repeat this procedure for all DNA fragments. **Hint**: Use loops

``` r
fragments <- c("141G6", "152F7", "230E8", "285E6")

res <- lapply(seq_along(fragments), function(x){

  tmp <- weight %>%
            select(contains(fragments[x])) %>%
            na.omit() %>%
            rename(tg     = paste(fragments[x], ".tg", sep = ""),
                   sex    = paste(fragments[x], ".sex", sep = ""),
                   age    = paste(fragments[x], ".age", sep = ""),
                   weight = paste(fragments[x], ".weight", sep = ""),
                   cage   = paste(fragments[x], ".cage", sep = ""),
                   line   = paste(fragments[x], ".line", sep = "")) %>%
            mutate(fragment = fragments[x])
})

weight <- do.call(rbind, res)
```

| line       | tg | sex | age | weight | cage | fragment |
| :--------- | -: | --: | --: | -----: | ---: | :------- |
| \#4-77-117 |  1 |   1 | 119 |   31.2 |    7 | 141G6    |
| \#4-77-118 |  1 |   1 | 119 |   28.4 |    7 | 141G6    |
| \#4-77-119 |  1 |   1 | 119 |   30.8 |    7 | 141G6    |
| \#4-77-120 |  1 |   1 | 119 |   30.3 |    7 | 141G6    |
| \#4-77-121 |  1 |   1 | 119 |   30.5 |    7 | 141G6    |
| \#4-77-122 |  1 |   1 | 119 |   31.4 |    7 | 141G6    |

We are almost done. Recall from above that `DNA` is encoded as:

  - 1 = 141G6
  - 2 = 152F7
  - 3 = 230E8
  - 4 = 285E6

We can use the function `recode` to achieve this:

``` r
weight <- weight %>%
            mutate(DNA = recode(fragment, "141G6"="1", "152F7"="2", "230E8"="3", "285E6"="4"))
```

| line       | tg | sex | age | weight | cage | fragment | DNA |
| :--------- | -: | --: | --: | -----: | ---: | :------- | :-- |
| \#4-77-117 |  1 |   1 | 119 |   31.2 |    7 | 141G6    | 1   |
| \#4-77-118 |  1 |   1 | 119 |   28.4 |    7 | 141G6    | 1   |
| \#4-77-119 |  1 |   1 | 119 |   30.8 |    7 | 141G6    | 1   |
| \#4-77-120 |  1 |   1 | 119 |   30.3 |    7 | 141G6    | 1   |
| \#4-77-121 |  1 |   1 | 119 |   30.5 |    7 | 141G6    | 1   |
| \#4-77-122 |  1 |   1 | 119 |   31.4 |    7 | 141G6    | 1   |

Finally, the last step is to merge the two datasets. Recall that we want
to use the `line` variable to merge the tables. Give it a try, call the
new table `dat`. **Hint**: Use the `left_join` function

``` r
dat <- weight %>%
          left_join(bp, by = "line") %>%
          select(DNA, line, tg, sex, age, weight, bp, cage, fragment)
```

| DNA | line       | tg | sex | age | weight |    bp | cage | fragment |
| :-- | :--------- | -: | --: | --: | -----: | ----: | ---: | :------- |
| 1   | \#4-77-117 |  1 |   1 | 119 |   31.2 | 126.8 |    7 | 141G6    |
| 1   | \#4-77-118 |  1 |   1 | 119 |   28.4 | 124.3 |    7 | 141G6    |
| 1   | \#4-77-119 |  1 |   1 | 119 |   30.8 | 124.7 |    7 | 141G6    |
| 1   | \#4-77-120 |  1 |   1 | 119 |   30.3 | 123.1 |    7 | 141G6    |
| 1   | \#4-77-121 |  1 |   1 | 119 |   30.5 | 123.4 |    7 | 141G6    |
| 1   | \#4-77-122 |  1 |   1 | 119 |   31.4 | 125.5 |    7 | 141G6    |

# Exercises

1.  Check the variable types of each column. Does it make sense? Should
    we change the variable type of any of the columns?

We can assess this by either printing the data frame or using the
`class`
    function

``` r
sapply(dat, class)
```

    ##         DNA        line          tg         sex         age      weight 
    ## "character" "character"   "numeric"   "numeric"   "numeric"   "numeric" 
    ##          bp        cage    fragment 
    ##   "numeric"   "numeric" "character"

Note that the varible `tg` is `numeric` but it only takes on either 0 or
1. We can convert this to either a character or a factor:

``` r
dat <- dat %>%
          mutate(tg = as.character(tg))
```

2.  Compute the number of mice in each DNA fragment group. Are the
    number of observations the same? **Hint**: Use `group_by` and
    `summarize`

<!-- end list -->

``` r
dat %>%
  group_by(DNA) %>%
  summarize(frequency = n()) 
```

| DNA | frequency |
| :-- | --------: |
| 1   |       177 |
| 2   |       158 |
| 3   |        37 |
| 4   |       160 |

The number of observations are **not** equal

3.  Above we used `weight %>% left_join(bp, by = "line")` to merge the
    two tables. What happens when you run `bp %>% left_join(weight, by =
    "line")`? What about `weight %>% right_join(bp, by = "line")`? Do we
    end up with the same results? **Hint**: What are the row numbers of
    the tables? Use `anyNA` and `na.omit`

Let’s take a look at the number of rows of each table:

``` r
tmp <- c(nrow(weight), nrow(bp))
names(tmp) <- c("weight", "bp")
tmp
```

    ## weight     bp 
    ##    532    547

The number of rows are different. This implies that when mergin, we have
to be aware of missing values. Notice that missing values occur when you
run either `bp %>% left_join(weight, by = "line")` or `weight %>%
right_join(bp, by = "line")`:

``` r
weight %>% 
  right_join(bp, by = "line") %>%
  anyNA()
```

    ## [1] TRUE

``` r
bp %>% 
  left_join(weight, by = "line") %>%
  anyNA()
```

    ## [1] TRUE

Actually `weight %>% right_join(bp, by = "line")` and `bp %>%
left_join(weight, by = "line")` returns the same result. We fix this by
removing `NA` with `na.omit`:

``` r
weight %>% 
  right_join(bp, by = "line") %>%
  anyNA()

bp %>% 
  left_join(weight, by = "line") %>%
  anyNA()
```

4.  In lab 1 we mutated the variable `fragment` if the the mouse did not
    have the DNA fragments of interest. This dataset does not have that.
    Write code to achieve this:

<!-- end list -->

``` r
dat <- dat %>%
          mutate(fragment == ifelse(tg == 0, "No trisomy", fragment))
```
