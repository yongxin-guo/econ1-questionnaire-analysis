Econ1 Students’ Feedbacks Analysis
================
Yongxin (Yong) GUO, s2093198

``` r
econ1 <- read_excel("econ1feed.xlsx")
```

``` r
econ1_1 <- econ1 %>% 
  mutate(
    `compare-school` = case_when(
      `compare-school` == "Better" ~ "Better",
      `compare-school` %in% c("Worse", "About the same", NA) ~ "Not better"
  ))
```

``` r
set.seed(1000)
boot_df_1 <- econ1_1 %>% 
  specify(response = `compare-school`, success = "Better") %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "prop")
```

``` r
boot_df_1 %>%
  summarize(lower = quantile(stat, 0.125),
            upper = quantile(stat, 0.875))
```

    ## # A tibble: 1 x 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1 0.105 0.211

We are 75% confident to say that amongth the current Economics 1
students, the proportion of students, who feel their experience at
School of Economics is better than other schools, is roughly between 11%
and 21%.

``` r
econ1_2 <- econ1 %>% 
  mutate(
    `compare-school` = case_when(
      `compare-school` == "Worse" ~ "Worse",
      `compare-school` %in% c("Better", "About the same", NA) ~ "Not worse"
  ))
```

``` r
set.seed(1000)
boot_df_2 <- econ1_2 %>% 
  specify(response = `compare-school`, success = "Not worse") %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "prop")
```

``` r
boot_df_2 %>%
  summarize(lower = quantile(stat, 0.125),
            upper = quantile(stat, 0.875))
```

    ## # A tibble: 1 x 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1 0.719 0.842

We are 75% confident to say that amongth the current Economics 1
students, the proportion of students, who feel their experience at
School of Economics is not worse than other schools, is roughly between
72% and 84%.

``` r
econ1_3 <- econ1 %>% 
  mutate(
    `compare-course` = case_when(
      `compare-course` == "Better" ~ "Better",
      `compare-course` %in% c("Worse", "About the same", NA) ~ "Not better"
  ))
```

``` r
set.seed(1000)
boot_df_3 <- econ1_3 %>% 
  specify(response = `compare-course`, success = "Better") %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "prop")
```

``` r
boot_df_3 %>%
  summarize(lower = quantile(stat, 0.125),
            upper = quantile(stat, 0.875))
```

    ## # A tibble: 1 x 2
    ##    lower upper
    ##    <dbl> <dbl>
    ## 1 0.0526 0.158

We are 75% confident to say that amongth the current Economics 1
students, the proportion of students, who feel their experience in
Economics 1 is better than other courses, is roughly between 5% and 16%.

``` r
econ1_4 <- econ1 %>% 
  mutate(
    `compare-course` = case_when(
      `compare-course` == "Worse" ~ "Worse",
      `compare-course` %in% c("Better", "About the same", NA) ~ "Not worse"
  ))
```

``` r
set.seed(1000)
boot_df_4 <- econ1_4 %>% 
  specify(response = `compare-course`, success = "Not worse") %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "prop")
```

``` r
boot_df_4 %>%
  summarize(lower = quantile(stat, 0.125),
            upper = quantile(stat, 0.875))
```

    ## # A tibble: 1 x 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1 0.719 0.842

We are 75% confident to say that amongth the current Economics 1
students, the proportion of students, who feel their experience in
Economics 1 is not worse than other courses, is roughly between 72% and
84%.

``` r
econ1_5 <- econ1 %>% 
  mutate(
    `compare-tut` = case_when(
      `compare-tut` == "Better" ~ "Better",
      `compare-tut` %in% c("Worse", "About the same", NA) ~ "Not better"
  ))
```

``` r
set.seed(1000)
boot_df_5 <- econ1_5 %>% 
  specify(response = `compare-tut`, success = "Better") %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "prop")
```

``` r
boot_df_5 %>%
  summarize(lower = quantile(stat, 0.125),
            upper = quantile(stat, 0.875))
```

    ## # A tibble: 1 x 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1 0.140 0.246

We are 75% confident to say that amongth the current Economics 1
students, the proportion of students, who feel their Economics 1
tutorial experience is better than tutorials of other courses, is
roughly between 14% and 25%.

``` r
econ1_6 <- econ1 %>% 
  mutate(
    `compare-tut` = case_when(
      `compare-tut` == "Worse" ~ "Worse",
      `compare-tut` %in% c("Better", "About the same", NA) ~ "Not worse"
  ))
```

``` r
set.seed(1000)
boot_df_6 <- econ1_6 %>% 
  specify(response = `compare-tut`, success = "Not worse") %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "prop")
```

``` r
boot_df_6 %>%
  summarize(lower = quantile(stat, 0.125),
            upper = quantile(stat, 0.875))
```

    ## # A tibble: 1 x 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1 0.789 0.895

We are 75% confident to say that amongth the current Economics 1
students, the proportion of students, who feel their Economics 1
tutorial experience is not worse than tutorials of other courses, is
roughly between 79% and 90%.
