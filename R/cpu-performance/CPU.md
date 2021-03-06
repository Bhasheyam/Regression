Linear Regression on CPU-Performance Dataset
================
Bhasheyam Krishnan

Loading the dataset

``` r
cpu = read.csv(file = "CPUPerformance.csv")
head(cpu)
```

    ##    Vendor MYCT MMIN  MMAX CACH CHMIN CHMAX Performance
    ## 1 adviser  125  256  6000  256    16   128         199
    ## 2  amdahl   29 8000 32000   32     8    32         253
    ## 3  amdahl   29 8000 32000   32     8    32         253
    ## 4  amdahl   29 8000 32000   32     8    32         253
    ## 5  amdahl   29 8000 16000   32     8    16         132
    ## 6  amdahl   26 8000 32000   64     8    32         290

``` r
anyNA(cpu)
```

    ## [1] FALSE

the dataset has no NA and missing values

``` r
summary(cpu)
```

    ##        Vendor         MYCT             MMIN            MMAX      
    ##  ibm      : 32   Min.   :  17.0   Min.   :   64   Min.   :   64  
    ##  nas      : 19   1st Qu.:  50.0   1st Qu.:  768   1st Qu.: 4000  
    ##  honeywell: 13   Median : 110.0   Median : 2000   Median : 8000  
    ##  ncr      : 13   Mean   : 203.8   Mean   : 2868   Mean   :11796  
    ##  sperry   : 13   3rd Qu.: 225.0   3rd Qu.: 4000   3rd Qu.:16000  
    ##  siemens  : 12   Max.   :1500.0   Max.   :32000   Max.   :64000  
    ##  (Other)  :107                                                   
    ##       CACH            CHMIN            CHMAX         Performance     
    ##  Min.   :  0.00   Min.   : 0.000   Min.   :  0.00   Min.   :  15.00  
    ##  1st Qu.:  0.00   1st Qu.: 1.000   1st Qu.:  5.00   1st Qu.:  28.00  
    ##  Median :  8.00   Median : 2.000   Median :  8.00   Median :  45.00  
    ##  Mean   : 25.21   Mean   : 4.699   Mean   : 18.27   Mean   :  99.33  
    ##  3rd Qu.: 32.00   3rd Qu.: 6.000   3rd Qu.: 24.00   3rd Qu.: 101.00  
    ##  Max.   :256.00   Max.   :52.000   Max.   :176.00   Max.   :1238.00  
    ## 

The above are the stats about the data and we see a huge various in the data. so data need to scalled

``` r
dim(cpu)
```

    ## [1] 209   8

For visualise data lets add index to the data.

``` r
ind = c(1:209)
cpu$index = ind
```

small dataset to with only 209 instance

``` r
library(ggplot2)
ggplot(cpu, aes(x = CACH, y = Performance)) + geom_line(color= "blue")+ geom_point(color ="red")+geom_hline(yintercept = mean(cpu$Performance), color = "green") + geom_hline(yintercept = min(cpu$Performance)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

Cache range 50 - 150 seems to have some higher perfomance. as most of the cpu are above average

``` r
library(ggplot2)

ggplot(cpu,aes(index,Performance)) + geom_line(color = "blue", size=1)+  geom_point(color = "red") + geom_hline(yintercept = mean(cpu$Performance), color = "green") + geom_hline(yintercept = min(cpu$Performance)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png) The above is an over all performance of all the cpu. lets see for each vendors

``` r
library(ggplot2)

ggplot(cpu,aes(index,Performance, color = Vendor)) + geom_line(size=0.5)+  geom_point() + geom_hline(yintercept = mean(cpu$Performance), color = "green") + geom_hline(yintercept = min(cpu$Performance)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

From the above see vendor Adviser an wang produce CPU with high Performance.

``` r
library(ggplot2)

ggplot(cpu,aes(index,MMAX)) + geom_line(color = "blue", size=1)+  geom_point(color = "red") + geom_hline(yintercept = mean(cpu$MMAX), color = "green") + geom_hline(yintercept = min(cpu$MMAX)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

Overall max memory for the cpu.

``` r
library(ggplot2)

ggplot(cpu,aes(index,MMAX, color = Vendor)) + geom_line(size=0.5)+  geom_point() + geom_hline(yintercept = mean(cpu$MMAX), color = "green") + geom_hline(yintercept = min(cpu$MMAX)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png) Adviser and wang having max memory as we expected. we can see the relation between mmax and performance.

``` r
library(ggplot2)

ggplot(cpu,aes(index,CACH)) + geom_line(color = "blue", size=1)+  geom_point(color = "red") + geom_hline(yintercept = mean(cpu$CACH), color = "green") + geom_hline(yintercept = min(cpu$CACH)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

``` r
library(ggplot2)

ggplot(cpu,aes(index,CACH, color = Vendor)) + geom_line(size=0.5)+  geom_point() + geom_hline(yintercept = mean(cpu$CACH), color = "green") + geom_hline(yintercept = min(cpu$CACH)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png) we can see from above cach is not contributing to the performance of CPU.

``` r
library(ggplot2)

ggplot(cpu,aes(index,CHMAX, color = Vendor)) + geom_line(size=0.5)+  geom_point() + geom_hline(yintercept = mean(cpu$CHMAX), color = "green") + geom_hline(yintercept = min(cpu$CHMAX)) +theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png) we cannot come to conclusion with this stats.

Now lets fit a regression model to see how the data behave

``` r
boxplot(Filter(is.numeric,cpu))
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

From the above we can see almost every features has some outlier

so lets normalize the data see how it works

``` r
calculatenorm=function(x){
  num=x-min(x)
  denom=max(x)-min(x)
  return (num/denom)
}

normalizeset= as.data.frame(lapply(Filter(is.numeric,cpu),calculatenorm))
nrow(normalizeset)
```

    ## [1] 209

``` r
boxplot(Filter(is.numeric,normalizeset))
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

``` r
summary(normalizeset)
```

    ##       MYCT              MMIN              MMAX              CACH        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.02225   1st Qu.:0.02204   1st Qu.:0.06156   1st Qu.:0.00000  
    ##  Median :0.06271   Median :0.06062   Median :0.12412   Median :0.03125  
    ##  Mean   :0.12598   Mean   :0.08780   Mean   :0.18350   Mean   :0.09846  
    ##  3rd Qu.:0.14026   3rd Qu.:0.12325   3rd Qu.:0.24925   3rd Qu.:0.12500  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##      CHMIN             CHMAX          Performance          index     
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00  
    ##  1st Qu.:0.01923   1st Qu.:0.02841   1st Qu.:0.01063   1st Qu.:0.25  
    ##  Median :0.03846   Median :0.04545   Median :0.02453   Median :0.50  
    ##  Mean   :0.09036   Mean   :0.10380   Mean   :0.06895   Mean   :0.50  
    ##  3rd Qu.:0.11538   3rd Qu.:0.13636   3rd Qu.:0.07032   3rd Qu.:0.75  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00

``` r
plot(normalizeset)
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

``` r
normalizeset = normalizeset[normalizeset$CHMAX < 0.50,]
nrow(normalizeset)
```

    ## [1] 202

``` r
boxplot(normalizeset)
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-20-1.png)

``` r
cor(normalizeset)
```

    ##                    MYCT        MMIN        MMAX        CACH       CHMIN
    ## MYCT         1.00000000 -0.33290350 -0.38996246 -0.33191910 -0.35407763
    ## MMIN        -0.33290350  1.00000000  0.80257789  0.61035706  0.72249644
    ## MMAX        -0.38996246  0.80257789  1.00000000  0.55148307  0.64252964
    ## CACH        -0.33191910  0.61035706  0.55148307  1.00000000  0.59861084
    ## CHMIN       -0.35407763  0.72249644  0.64252964  0.59861084  1.00000000
    ## CHMAX       -0.35165150  0.42105707  0.47209676  0.41224472  0.57049932
    ## Performance -0.29575141  0.92070771  0.87872071  0.67163345  0.74572152
    ## index       -0.08923967 -0.05003738 -0.03403998  0.04672528 -0.05065647
    ##                  CHMAX Performance       index
    ## MYCT        -0.3516515 -0.29575141 -0.08923967
    ## MMIN         0.4210571  0.92070771 -0.05003738
    ## MMAX         0.4720968  0.87872071 -0.03403998
    ## CACH         0.4122447  0.67163345  0.04672528
    ## CHMIN        0.5704993  0.74572152 -0.05065647
    ## CHMAX        1.0000000  0.47506140 -0.22730993
    ## Performance  0.4750614  1.00000000 -0.08078276
    ## index       -0.2273099 -0.08078276  1.00000000

From the above we can see, we cannot see any feature pair are highly corelated

``` r
normalizeset = subset(normalizeset,select = c("MMIN","MMAX" ,"CACH","CHMIN", "CHMAX", "Performance"))
```

``` r
cputrain = normalizeset
```

``` r
cpufit = lm(Performance ~ . , data = cputrain)
summary(cpufit)
```

    ## 
    ## Call:
    ## lm(formula = Performance ~ ., data = cputrain)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.067291 -0.010826  0.001904  0.014499  0.265688 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.032992   0.003460  -9.536  < 2e-16 ***
    ## MMIN         0.434580   0.034470  12.608  < 2e-16 ***
    ## MMAX         0.231085   0.023016  10.040  < 2e-16 ***
    ## CACH         0.090315   0.020692   4.365 2.06e-05 ***
    ## CHMIN        0.092986   0.037684   2.468   0.0145 *  
    ## CHMAX       -0.002035   0.033860  -0.060   0.9521    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03083 on 196 degrees of freedom
    ## Multiple R-squared:  0.9174, Adjusted R-squared:  0.9153 
    ## F-statistic: 435.3 on 5 and 196 DF,  p-value: < 2.2e-16

From the above foolowing are the observations: we can eliminate the CHMIN and CHMAX as they have higher P - Value R- Square and Adj- R- Square are Very high seems like overfit

``` r
cpufit1 = lm(Performance ~  MMIN +MMAX + CACH  , data = cputrain)
summary(cpufit1)
```

    ## 
    ## Call:
    ## lm(formula = Performance ~ MMIN + MMAX + CACH, data = cputrain)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.073649 -0.011503  0.000896  0.015038  0.272430 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.031262   0.003247  -9.628  < 2e-16 ***
    ## MMIN         0.468331   0.032090  14.594  < 2e-16 ***
    ## MMAX         0.237977   0.022678  10.494  < 2e-16 ***
    ## CACH         0.105049   0.020065   5.235 4.18e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03121 on 198 degrees of freedom
    ## Multiple R-squared:  0.9145, Adjusted R-squared:  0.9132 
    ## F-statistic: 705.5 on 3 and 198 DF,  p-value: < 2.2e-16

``` r
par(mfrow = c(2, 2)) 
plot(cpufit) 
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-26-1.png)

``` r
par(mfrow = c(2, 2))  
plot(cpufit1) 
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-27-1.png)

``` r
cputrain$predicted = predict(cpufit1)
cputrain$Resudials = residuals(cpufit1)
```

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
cputrain %>% select(Performance, predicted, Resudials) %>%head()
```

    ##   Performance predicted    Resudials
    ## 2  0.19460343 0.2171169 -0.022513471
    ## 3  0.19460343 0.2171169 -0.022513471
    ## 4  0.19460343 0.2171169 -0.022513471
    ## 5  0.09566639 0.1575631 -0.061896721
    ## 6  0.22485691 0.2302480 -0.005391139
    ## 7  0.29926410 0.3475654 -0.048301269

``` r
library(ggplot2)
ggplot(cputrain, aes(x =  MMIN + MMAX + CACH, y = Performance )) +
  geom_point(color = "Blue") + theme_bw()
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-30-1.png)

``` r
ggplot(cputrain, aes(x =  MMIN + MMAX + CACH, y = Performance ))+ geom_segment(aes(xend = MMIN + MMAX + CACH, yend = predicted), alpha = .5)+geom_smooth(method = "lm", se = FALSE, color = "black") +geom_point(color = "blue")+ geom_point(aes(y = predicted), shape = 1, color =" red") 
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-31-1.png)

The above is the difference between the actual points Blue and Predicited Points Red and the linear regression lm is show in Black color

``` r
barplot(cputrain$Resudials)
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-32-1.png) From the above we can see the resudial is somewhat evenly distributed also we can find negative effict which will detect prediction with lower than the actual. which actually better than to have higher performance result and lower performance in actual.

``` r
ggplot(cputrain, aes(x =  MMIN + MMAX + CACH, y = Performance ))+ geom_segment(aes(xend = MMIN + MMAX + CACH, yend = predicted), alpha = .5)+geom_point(aes(color = abs(Resudials), size = abs(Resudials))) + # size also mapped
  scale_color_continuous(low = "black", high = "green") +geom_smooth(method = "lm", se = FALSE, color = "black") + geom_point(aes(y = predicted), shape = 1, color =" blue") 
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-33-1.png)

The above plot is visuvialsation of Residuvals

``` r
library("magrittr")
```

    ## Warning: package 'magrittr' was built under R version 3.4.3

``` r
library("tidyr")
```

    ## Warning: package 'tidyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     extract

``` r
cputrain %>% gather (-Performance,-CHMAX, -CHMIN,-predicted, -Resudials,key = "iv", value = "x" ) %>% 
  
  ggplot( aes(x =  x, y = Performance ))+ geom_segment(aes(xend = x, yend = predicted), alpha = .5)+geom_point(aes(color = abs(Resudials), size = abs(Resudials))) + # size also mapped
  scale_color_continuous(low = "black", high = "green") +geom_smooth(method = "lm", se = FALSE, color = "black") + geom_point(aes(y = predicted), shape = 1, color =" blue") + facet_grid(~ iv , scales = "free_x") 
```

![](CPU_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-34-1.png)

From the above we can see fitting the features seperately and see how it works. all works almost similar. so linear model fitted with these feature may predict the performance of the CPU.
