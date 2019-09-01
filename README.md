-   [Introduction](#introduction)
-   [A/B Testing using T-test and Permutation Test](#ab-testing-using-t-test-and-permutation-test)
    -   [**<span style="color:red">Problem 1: Google or S&P 500?</span>**](#problem-1-google-or-sp-500)
    -   [Calculating the Quarterly-return](#calculating-the-quarterly-return)
-   [A/B Testing using logistic regression](#ab-testing-using-logistic-regression)
    -   [<span style="color:blue">**Problem 2: New page or old page? **</span>](#problem-2-new-page-or-old-page)
    -   [Exploring the Data Set](#exploring-the-data-set)
    -   [Plotting results](#plotting-results)
    -   [Analyzing results](#analyzing-results)

Introduction
------------

In this work, we are going to present two problems. In this first one, our response variable will be numerical, so we will apply A/B test using t-test and a permutation test. In the second problem, our response variable will be categorical, so we will apply logistic regression. Note that we will use the packages `powerMediation`, `pwr` and `infer` for these tests in addition to `tidyverse`, `broom` and `ggplot2`.

A/B Testing using T-test and Permutation Test
---------------------------------------------

### **<span style="color:red">Problem 1: Google or S&P 500?</span>**

In this section, we will first work on the question: If one invests $100 daily, does he make more or less money in average by investing his money on Google stock than S&P 500? Or the difference in the means is not significant enough. Secondly, we will change "daily" to "quarterly" (3 monthly) and answer the same question.

#### Stating the null and alternative hypotheses:

Let's say the mean daily\_return for google is *μ*<sub>1</sub> and snp500 is *μ*<sub>2</sub>. Then our null and alternative hypotheses are:

*H*<sub>0</sub>: *μ*<sub>1</sub> − *μ*<sub>2</sub> = 0

*H*<sub>*A*</sub>: *μ*<sub>1</sub> − *μ*<sub>2</sub> ≠ 0

#### Constructing the data set

Using the websites: <https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC> <https://finance.yahoo.com/quote/GOOG/history?p=GOOG>&.tsrc=fin-srch, we downloaded the last 10 years of the stock price data for Google and S&P 500.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.5
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.2.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
google_data<-read.csv("GOOG.csv")
glimpse(google_data)
```

    ## Observations: 2,517
    ## Variables: 7
    ## $ Date      <fct> 2009-08-31, 2009-09-01, 2009-09-02, 2009-09-03, 2009...
    ## $ Open      <dbl> 229.0365, 228.9817, 227.0589, 227.0589, 227.9307, 23...
    ## $ High      <dbl> 230.0677, 232.5384, 228.3092, 228.2694, 230.4363, 23...
    ## $ Low       <dbl> 228.1449, 226.3615, 225.4500, 226.6505, 227.0390, 22...
    ## $ Close     <dbl> 229.9730, 227.0290, 225.6592, 227.9057, 229.7887, 22...
    ## $ Adj.Close <dbl> 229.9730, 227.0290, 225.6592, 227.9057, 229.7887, 22...
    ## $ Volume    <int> 3930400, 5209100, 3623100, 3304600, 3009500, 5333300...

``` r
snp500_data<-read.csv("GSPC.csv")
glimpse(snp500_data)
```

    ## Observations: 2,517
    ## Variables: 7
    ## $ Date      <fct> 8/31/09, 9/1/09, 9/2/09, 9/3/09, 9/4/09, 9/8/09, 9/9...
    ## $ Open      <dbl> 1025.21, 1019.52, 996.07, 996.12, 1003.84, 1018.67, ...
    ## $ High      <dbl> 1025.21, 1028.45, 1000.34, 1003.43, 1016.48, 1026.07...
    ## $ Low       <dbl> 1014.62, 996.28, 991.97, 992.25, 1001.65, 1018.67, 1...
    ## $ Close     <dbl> 1020.62, 998.04, 994.75, 1003.24, 1016.40, 1025.39, ...
    ## $ Adj.Close <dbl> 1020.62, 998.04, 994.75, 1003.24, 1016.40, 1025.39, ...
    ## $ Volume    <dbl> 5004560000, 6862360000, 5842730000, 4624280000, 4097...

#### Defining a new variable

We are going to define a new variable `daily_return`. The formula we are going to use is as follows:

$$\\text{daily\_return}=\\frac{Close}{Open}\*100-100$$
 We are going to add this new feature to our data sets: `google_data` and `snp500_data` and name them as `google_data_added` and `snp500_data_added`.

``` r
google_data_added<-google_data%>%
  mutate(daily_return=Close/Open*100-100)
```

    ## Warning: The `printer` argument is deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

``` r
glimpse(google_data_added)
```

    ## Observations: 2,517
    ## Variables: 8
    ## $ Date         <fct> 2009-08-31, 2009-09-01, 2009-09-02, 2009-09-03, 2...
    ## $ Open         <dbl> 229.0365, 228.9817, 227.0589, 227.0589, 227.9307,...
    ## $ High         <dbl> 230.0677, 232.5384, 228.3092, 228.2694, 230.4363,...
    ## $ Low          <dbl> 228.1449, 226.3615, 225.4500, 226.6505, 227.0390,...
    ## $ Close        <dbl> 229.9730, 227.0290, 225.6592, 227.9057, 229.7887,...
    ## $ Adj.Close    <dbl> 229.9730, 227.0290, 225.6592, 227.9057, 229.7887,...
    ## $ Volume       <int> 3930400, 5209100, 3623100, 3304600, 3009500, 5333...
    ## $ daily_return <dbl> 0.4088837, -0.8527681, -0.6164695, 0.3729503, 0.8...

``` r
snp500_data_added<-snp500_data%>%
  mutate(daily_return=Close/Open*100-100)
glimpse(snp500_data_added)
```

    ## Observations: 2,517
    ## Variables: 8
    ## $ Date         <fct> 8/31/09, 9/1/09, 9/2/09, 9/3/09, 9/4/09, 9/8/09, ...
    ## $ Open         <dbl> 1025.21, 1019.52, 996.07, 996.12, 1003.84, 1018.6...
    ## $ High         <dbl> 1025.21, 1028.45, 1000.34, 1003.43, 1016.48, 1026...
    ## $ Low          <dbl> 1014.62, 996.28, 991.97, 992.25, 1001.65, 1018.67...
    ## $ Close        <dbl> 1020.62, 998.04, 994.75, 1003.24, 1016.40, 1025.3...
    ## $ Adj.Close    <dbl> 1020.62, 998.04, 994.75, 1003.24, 1016.40, 1025.3...
    ## $ Volume       <dbl> 5004560000, 6862360000, 5842730000, 4624280000, 4...
    ## $ daily_return <dbl> -0.4477099, -2.1068779, -0.1325215, 0.7147728, 1....

Now we are going to construct a new data frame `df` that has both the variable `daily_return` and the condition that is either `google` or `snp500`.

``` r
df1 = data.frame(daily_return = google_data_added$daily_return, condition = rep("google",nrow(google_data_added)))
df2 = data.frame(daily_return = snp500_data_added$daily_return, condition = rep("snp500",nrow(snp500_data_added)))

df<-rbind(df1,df2)
glimpse(df)
```

    ## Observations: 5,034
    ## Variables: 2
    ## $ daily_return <dbl> 0.4088837, -0.8527681, -0.6164695, 0.3729503, 0.8...
    ## $ condition    <fct> google, google, google, google, google, google, g...

#### pwr.t.test()

First, let's check how many observations we should have for a test with 80% power. We need `pwr` package to find that number.

``` r
# Load package to run power analysis
library(pwr)

# Run power analysis for t-test
sample_size <- pwr.t.test(d =0.5,
                          sig.level = 0.05,
                          power = 0.8)
sample_size
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 63.76561
    ##               d = 0.5
    ##       sig.level = 0.05
    ##           power = 0.8
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

We see that we should have at least 64 observations for each condition. We have 2517 observations for each condition.

Now, let's plot the histograms for each condition and see how their distributions look like.

``` r
ggplot(df, mapping = aes(x=daily_return)) +
  # Add a histogram layer
  geom_histogram() +
  # Facet by class
  facet_wrap(~condition)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](AB_Testing_logistic_ttest_files/figure-markdown_github/unnamed-chunk-6-1.png)

Looking at the histograms, we can tell that they do not look like very normal and the variability in "snp500" looks smaller than "google". We can calculate the means and the standard deviations as follows:

``` r
df %>%
  # Group by class
  group_by(condition) %>%
  # Calculate the std dev of wordsum as std_dev_wordsum
  summarize(mean=mean(daily_return),std_dev_responses=sd(daily_return))
```

    ## # A tibble: 2 x 3
    ##   condition     mean std_dev_responses
    ##   <fct>        <dbl>             <dbl>
    ## 1 google    -0.00658             1.17 
    ## 2 snp500     0.0324              0.871

As we calculated the standard deviation for each condition, we got unequal standard deviations. Note that in R, the default setting in t.test is var.equal = FALSE which means it assumes the samples could have different standard deviations, so we will not violate that assumption.

#### Applying t-test

Let's use the `t.test()` function, setting the dependent variable to the column referring to `daily_return` and the independent variable to the column for `condition`.

``` r
# Run t-test
experiment_results <-t.test(daily_return ~ condition,
                                data = df)
experiment_results
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  daily_return by condition
    ## t = -1.3388, df = 4645.4, p-value = 0.1807
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.09602589  0.01809423
    ## sample estimates:
    ## mean in group google mean in group snp500 
    ##         -0.006575446          0.032390384

Since the p-value=0.1807 is bigger than the significance level=0.05, we do not have enough evidence to reject the null hypothesis, implies that it can't be said that there is a significant difference between the means of daily returns of "google" and "spn500" for the years 2009-2019.

#### T-test vs. linear regression

We will use linear regression and compare our results with previously obtained t-test results.

``` r
lm(daily_return ~ condition, data = df) %>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = daily_return ~ condition, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0062 -0.4747  0.0165  0.5310  5.2072 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)     -0.006575   0.020581  -0.319    0.749
    ## conditionsnp500  0.038966   0.029105   1.339    0.181
    ## 
    ## Residual standard error: 1.033 on 5032 degrees of freedom
    ## Multiple R-squared:  0.0003561,  Adjusted R-squared:  0.0001574 
    ## F-statistic: 1.792 on 1 and 5032 DF,  p-value: 0.1807

Looking at the statistics of `lm` model, we get the same conclusion since the p-values are bigger than the significance level.

Note that even though we applied `lm`, we shouldn't have had. Because when we look at *the normal probablity plot of residuals* , as seen below, we end up with an "s" shape, which is far from the solid line at the tails. This implies that the residuals are not normal so it violates the assumption for the `lm` model.

``` r
daily_return.lm=lm(daily_return ~ condition, data=df) 
daily_return.stdres=rstandard(daily_return.lm)
qqnorm(daily_return.stdres)
qqline(daily_return.stdres)
```

![](AB_Testing_logistic_ttest_files/figure-markdown_github/unnamed-chunk-10-1.png)

#### Using `infer` package to perform a permentation test for the difference between mean `daily_return` for `google` and `snp500`

Besides traditional models like t-test and `lm` model, \[since some required assumptions were not satisfied\], we will now use `infer` package to perform a permutation test for the difference between mean `daily_return` for `google` and `snp500`.

First, we start by finding the observed mean difference. The following R-code helps us find that difference.

``` r
# Calculate observed difference in means
 mean_daily_return<-df %>%
  # Group by habit group
  group_by(condition) %>%
  # Calculate mean weight for each group
  summarize(mean_daily_returns = mean(daily_return)) 
mean_daily_return
```

    ## # A tibble: 2 x 2
    ##   condition mean_daily_returns
    ##   <fct>                  <dbl>
    ## 1 google              -0.00658
    ## 2 snp500               0.0324

``` r
diff_mean_obs<- mean_daily_return$mean_daily_returns[1]- mean_daily_return$mean_daily_returns[2]
diff_mean_obs
```

    ## [1] -0.03896583

The observed mean difference is found as -0.03896583.

Now we are going to construct a null distribution using `permute`. In the following R-code, we generate 1500 samples and the `stat` are the mean differences between the two conditions for 1500 samples.

``` r
library(infer)
set.seed(123)
n_replicates <- 1500
# Generate 1500 differences in means via randomization
diff_mean_ht <- df %>% 
  # Specify daily_return vs. condition
  specify(daily_return~condition) %>% 
  # Null = no difference between means
  hypothesize(null="independence") %>% 
  # Shuffle labels 1500 times
  generate(reps=1500, type="permute") %>%
  # Calculate test statistic, google then snp500
  calculate(stat="diff in means", c("google", "snp500"))
```

    ## Warning: `lgl_len()` is deprecated as of rlang 0.2.0.
    ## Please use `new_logical()` instead.
    ## This warning is displayed once per session.

    ## Warning: `is_lang()` is deprecated as of rlang 0.2.0.
    ## Please use `is_call()` instead.
    ## This warning is displayed once per session.

    ## Warning: `lang()` is deprecated as of rlang 0.2.0.
    ## Please use `call2()` instead.
    ## This warning is displayed once per session.

    ## Warning: `new_overscope()` is deprecated as of rlang 0.2.0.
    ## Please use `new_data_mask()` instead.
    ## This warning is displayed once per session.

``` r
# See the result
head(diff_mean_ht)
```

    ## # A tibble: 6 x 2
    ##   replicate     stat
    ##       <int>    <dbl>
    ## 1         1 -0.0108 
    ## 2         2  0.0233 
    ## 3         3  0.0364 
    ## 4         4  0.00119
    ## 5         5 -0.0295 
    ## 6         6  0.00464

#### Computing p-value

Since we have a two-tail test, we will find the area where the stat is less than the observed data and then we will multiply it by two. The following code does it for us.

``` r
# pvalue 

diff_mean_ht %>%
  # Identify simulated test statistics at least as extreme as observed
  filter(stat<diff_mean_obs) %>%
  # Calculate p-value
  summarize(
    one_sided_p_val = n()/n_replicates,
    two_sided_p_val = 2*one_sided_p_val)
```

    ## # A tibble: 1 x 2
    ##   one_sided_p_val two_sided_p_val
    ##             <dbl>           <dbl>
    ## 1          0.0893           0.179

The two-sided p-value is calculated as 0.1786 which is bigger than the significance level, so we do not have enough evidence to reject the null hypothesis. That means that we do not have enough evidence to reject the null hypothesis: there is no significant difference between the daily return means of google and snp500.

### Calculating the Quarterly-return

``` r
google_data_reduced<-google_data[seq(1,2517,60),] 
google_data_q<-google_data_reduced%>%
  mutate(monthly_return=Close/Open*100-100)
glimpse(google_data_q)
```

    ## Observations: 42
    ## Variables: 8
    ## $ Date           <fct> 2009-08-31, 2009-11-24, 2010-02-23, 2010-05-19,...
    ## $ Open           <dbl> 229.0365, 290.1724, 270.4861, 247.2034, 243.587...
    ## $ High           <dbl> 230.0677, 291.0540, 270.8000, 248.7875, 244.677...
    ## $ Low            <dbl> 228.1449, 287.1935, 265.1512, 242.9593, 242.097...
    ## $ Close          <dbl> 229.9730, 290.4563, 266.5360, 246.2918, 242.266...
    ## $ Adj.Close      <dbl> 229.9730, 290.4563, 266.5360, 246.2918, 242.266...
    ## $ Volume         <int> 3930400, 3222400, 5766600, 6917100, 3883800, 42...
    ## $ monthly_return <dbl> 0.40888371, 0.09785081, -1.46040567, -0.3687550...

``` r
snp500_data_reduced<-snp500_data[seq(1,2517,60),] #picks the data at 3, 6.. months. (assume 1 month has 20 business days)
snp500_data_q<-snp500_data_reduced%>%
  mutate(monthly_return=Close/Open*100-100)
glimpse(snp500_data_q)
```

    ## Observations: 42
    ## Variables: 8
    ## $ Date           <fct> 8/31/09, 11/24/09, 2/23/10, 5/19/10, 8/13/10, 1...
    ## $ Open           <dbl> 1025.21, 1105.83, 1107.49, 1119.57, 1082.22, 12...
    ## $ High           <dbl> 1025.21, 1107.56, 1108.58, 1124.27, 1086.25, 12...
    ## $ Low            <dbl> 1014.62, 1097.63, 1092.18, 1100.66, 1079.00, 12...
    ## $ Close          <dbl> 1020.62, 1105.65, 1094.60, 1115.05, 1079.25, 12...
    ## $ Adj.Close      <dbl> 1020.62, 1105.65, 1094.60, 1115.05, 1079.25, 12...
    ## $ Volume         <dbl> 5004560000, 3700820000, 4521050000, 6765800000,...
    ## $ monthly_return <dbl> -0.4477098521, -0.0162712177, -1.1638944023, -0...

Now we are going to construct a new data frame `dfq` that has the variable `monthly_return` and the stock it belongs to.

``` r
df1q = data.frame(monthly_return = google_data_q$monthly_return, condition = rep("google",nrow(google_data_q)))
df2q = data.frame(monthly_return = snp500_data_q$monthly_return, condition = rep("snp500",nrow(snp500_data_q)))

dfq<-rbind(df1q,df2q)
glimpse(dfq)
```

    ## Observations: 84
    ## Variables: 2
    ## $ monthly_return <dbl> 0.40888371, 0.09785081, -1.46040567, -0.3687550...
    ## $ condition      <fct> google, google, google, google, google, google,...

#### Applying t-test

Let's use the `t.test()` function, setting the dependent variable to the column referring to `daily_return` and the independent variable to the column for `condition`.

``` r
# Run t-test
experiment_results_q <-t.test(monthly_return ~ condition,
                                data = dfq)
experiment_results_q
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  monthly_return by condition
    ## t = -0.024701, df = 73.309, p-value = 0.9804
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4461551  0.4352306
    ## sample estimates:
    ## mean in group google mean in group snp500 
    ##           -0.1471529           -0.1416907

As in the previous case, the statistcis show us that we do not have enough evidence to reject the null hypothesis, implies that it can't be said that there is a significant difference between the means of quarterly returns of "google" and "spn500" for the years 2009-2019.

A/B Testing using logistic regression
-------------------------------------

### <span style="color:blue">**Problem 2: New page or old page? **</span>

In this section, we will investigate two versions of a single variable \[in this case it is `group`\], by testing a subject's response \[in this case it is `converted` and it is a categorical variable\] to variant A \[`control`\] against variant B \[`treatment`\]. After doing the analysis, we will determine which of the two variants is more effective. First, let's start by exploring the data.

### Exploring the Data Set

Let's glimpse our dataset: `ab_data.csv` by using `glimpse` function of `tidyverse` package.

``` r
library(tidyverse)
ab_data<-read.csv("ab_data.csv")
glimpse(ab_data)
```

    ## Observations: 294,478
    ## Variables: 5
    ## $ user_id      <int> 851104, 804228, 661590, 853541, 864975, 936923, 6...
    ## $ timestamp    <fct> 1/21/17, 1/12/17, 1/11/17, 1/8/17, 1/21/17, 1/10/...
    ## $ group        <fct> control, control, treatment, treatment, control, ...
    ## $ landing_page <fct> old_page, old_page, new_page, new_page, old_page,...
    ## $ converted    <int> 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0...

We have 294,478 observations and 5 variables. Let's see how many observations we have in the control and treatment group. Notice that `group` and `landing_page` are dependent variables. We can see that by using `cor` function as follows:

``` r
ab_data$group2<-ifelse(ab_data$group=="control", 0, 1)
ab_data$landing_page2<-ifelse(ab_data$landing_page=="old_page", 0, 1)
```

``` r
cor(ab_data$group2,ab_data$landing_page2)
```

    ## [1] 0.97356

The reason why we didn't get the correlation as equal to 1 is we believe that there are some typos in the data set. So will focus on the variable `group` that has the control and the test data, not the `landing_page`.

``` r
ab_data %>%
  filter((group=="control" & landing_page=="new_page")|(group=="treatment" & landing_page=="old_page"))%>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  3893

We believe that 3893 out of 294,478 observations had typos.

``` r
table(ab_data$group  )
```

    ## 
    ##   control treatment 
    ##    147202    147276

We see that the number of observations in the control group is almost the same as the number of observations in the treatment group. We can say that the split is 50-50%.

#### Baseline Conversion Rates

First, let's divide our data set into two where the first one has the control group and the second is the treatment group. We will calculate baseline conversion rates for the control group.

``` r
ab_data_control<-ab_data %>%
   filter(group =="control")
head(ab_data_control)
```

    ##   user_id timestamp   group landing_page converted group2 landing_page2
    ## 1  851104   1/21/17 control     old_page         0      0             0
    ## 2  804228   1/12/17 control     old_page         0      0             0
    ## 3  864975   1/21/17 control     old_page         1      0             0
    ## 4  936923   1/10/17 control     old_page         0      0             0
    ## 5  719014   1/17/17 control     old_page         0      0             0
    ## 6  644214   1/22/17 control     old_page         1      0             0

Let's calculate the conversion rate for our control group.

``` r
ab_control_sum_all<-ab_data_control %>%
   summarize(conversion_rate = mean(converted))
ab_control_sum_all
```

    ##   conversion_rate
    ## 1       0.1203992

#### Current conversion rate by day of a week

Let's start by computing our pre-experiment conversion rate as a baseline. Let's compute it by day of a week. Since the month is January in all observations, it makes no sense to group by month.

Using the function `weekday()` from the package `lubridate`, we can compute the conversion rate by month. Take a look at the data to see how much conversion rates vary throughout the week.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
ab_base_sum<-ab_data_control%>%
  group_by(weekdays(mdy(timestamp))) %>%
  summarize(conversion_rate = mean(converted))
ab_base_sum
```

    ## # A tibble: 7 x 2
    ##   `weekdays(mdy(timestamp))` conversion_rate
    ##   <chr>                                <dbl>
    ## 1 Friday                               0.116
    ## 2 Monday                               0.123
    ## 3 Saturday                             0.124
    ## 4 Sunday                               0.120
    ## 5 Thursday                             0.121
    ## 6 Tuesday                              0.117
    ## 7 Wednesday                            0.122

``` r
library(ggplot2)
ggplot(ab_base_sum, aes(x = `weekdays(mdy(timestamp))`, y = conversion_rate)) +
geom_point() +
geom_line()
```

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](AB_Testing_logistic_ttest_files/figure-markdown_github/unnamed-chunk-27-1.png)

It seems like the conversion rate is the highest on Saturday and lowest on Friday.

### Plotting results

We are going to stary this section by plotting the two conversion rates, one for the treatment condition and one for the control condition.

``` r
# Group and summarize data
aba_data_sum <- ab_data %>%
  group_by(group, weekdays(mdy(timestamp))) %>%
  summarize(conversion_rate = mean(converted))
aba_data_sum
```

    ## # A tibble: 14 x 3
    ## # Groups:   group [?]
    ##    group     `weekdays(mdy(timestamp))` conversion_rate
    ##    <fct>     <chr>                                <dbl>
    ##  1 control   Friday                               0.116
    ##  2 control   Monday                               0.123
    ##  3 control   Saturday                             0.124
    ##  4 control   Sunday                               0.120
    ##  5 control   Thursday                             0.121
    ##  6 control   Tuesday                              0.117
    ##  7 control   Wednesday                            0.122
    ##  8 treatment Friday                               0.118
    ##  9 treatment Monday                               0.120
    ## 10 treatment Saturday                             0.117
    ## 11 treatment Sunday                               0.118
    ## 12 treatment Thursday                             0.118
    ## 13 treatment Tuesday                              0.122
    ## 14 treatment Wednesday                            0.119

``` r
# Make plot of conversion rates over time
ggplot(aba_data_sum,
       aes(x = `weekdays(mdy(timestamp))`,
           y = conversion_rate,
           color = group,
           group = group)) +
  geom_point() +
  geom_line()+
labs(x = "Day",
       y = "Conversion Rate")
```

![](AB_Testing_logistic_ttest_files/figure-markdown_github/unnamed-chunk-29-1.png)

### Analyzing results

To analyze our results, we will use the function `glm()` and set family to "binomial". \[since the variable `converted` is a categorical variable. \]

``` r
# Load package for cleaning model results
library(broom)
ab_data %>%
  group_by(group) %>%
  summarize(conversion_rate = mean(converted))
```

    ## # A tibble: 2 x 2
    ##   group     conversion_rate
    ##   <fct>               <dbl>
    ## 1 control             0.120
    ## 2 treatment           0.119

``` r
# Run logistic regression
results <- glm(converted ~ group,
                          family = "binomial",
                          data = ab_data) %>%
tidy()
results
```

    ##             term    estimate   std.error   statistic   p.value
    ## 1    (Intercept) -1.98865547 0.008009129 -248.298579 0.0000000
    ## 2 grouptreatment -0.01404597 0.011355552   -1.236925 0.2161149

We found that there is not a significant difference between the groups "control" and "treatment" since our p-value was about 0.21, which is not less than 0.05.

#### Power analysis Tuesday

Now we will run a power analysis assuming we were going to run the experiment on Tuesday. The reason why we picked that day is it is the only day treatment group has a higher conversion rate than the control group. To get a significant difference between the control and the treatment groups, we will find the smallest size to run our experiment. In order to do that, we will use `powerMediation` package. Remember that the conversion rate on Tuesday for the control group is 0.1166688. Assume our desired conversion rate for the treatment group is 0.123, then we compute and look at sample size as follows:

``` r
library(powerMediation)
# Compute and look at sample size for the experiment on Tuesdays
total_sample_size <- SSizeLogisticBin(p1 = 0.116, # rounded conversion rate for Tuesday control group
                                      p2 = 0.125,
                                      B = 0.5, 
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
```

    ## [1] 41076

We now know if we ran the experiment on Tuesday we would need at least 41076 data points or 20538 per group.

#### <span style="color:blue">*Sample Size for Tuesday in the data set*</span>

First, let's count the number of observations made on Tuesday.

``` r
ab_count_Tuesday<-ab_data_control%>%
  filter(weekdays(mdy(timestamp))=="Tuesday")%>%
  count()
ab_count_Tuesday
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1 23931

We have 23931 observations for Tuesday in the control group which is more than the minimum number required 20538.

``` r
# Load package for cleaning model results
library(broom)
ab_data_Tuesday<-ab_data %>%
 filter(weekdays(mdy(timestamp))=="Tuesday")

# Run logistic regression
results <- glm(converted ~ group,
                          family = "binomial",
                          data = ab_data_Tuesday) %>%
tidy()
results
```

    ##             term    estimate  std.error  statistic    p.value
    ## 1    (Intercept) -2.02436149 0.02013609 -100.53398 0.00000000
    ## 2 grouptreatment  0.05221811 0.02822265    1.85022 0.06428184

Looking at the statistics, we can't say there is a significant difference between the groups "control" and "treatment" even on Tuesday since our p-value was about 0.06428184, which is not less than 0.05.
