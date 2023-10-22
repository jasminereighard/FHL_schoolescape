``` r
#download data

#all data
cym_dat <- read.csv("data/rawdata_allcombinedSep28.csv")

#NND data
nnd_df <- read.csv("data/rawdata_combined_NND_9.28.csv")

#spontaneous turns
spon_turn <- read.csv("data/sp_turn_10-9-23.csv")

#categorize by s and w
cym_dat$s_w_reactor <- ifelse(substr(cym_dat$responder, 1, 1) == "s", "s", "w")


wave_dat <- subset(cym_dat, substr(responder, 1, 1) == "w")

#make sure variables are as they are supposed to be

cym_dat$s_w_reactor <- as.factor(cym_dat$s_w_reactor)
cym_dat$size <- as.factor(cym_dat$size)
cym_dat$school <- as.factor(cym_dat$school)
cym_dat$stimulus <- as.factor(cym_dat$stimulus)
```

``` r
#Define thresholds for fast and slow responses for small and large

#look at averages
turnrate_result <- aggregate(turning_rate ~ size, data = spon_turn, FUN = mean)
print(turnrate_result)
```

    ##    size turning_rate
    ## 1 large    0.2243052
    ## 2 small    0.2435468

``` r
#large 0.2243052
#small 0.243546

#Mann-Whitney test
manwhit_spon <- wilcox.test(turning_rate ~ size, data = spon_turn)
print(manwhit_spon) 
```

    ## 
    ##  Wilcoxon rank sum exact test
    ## 
    ## data:  turning_rate by size
    ## W = 213, p-value = 0.6281
    ## alternative hypothesis: true location shift is not equal to 0

``` r
# p-value is less not less than 0.05, conclusion: there is no statistically significant difference in the distributions of "turning_rate" between the two size.


spon_anova <- aov(turning_rate ~ size, data = spon_turn)
print(summary(spon_anova))
```

    ##             Df Sum Sq  Mean Sq F value Pr(>F)
    ## size         1 0.0039 0.003938   0.356  0.554
    ## Residuals   42 0.4649 0.011068

``` r
plot(spon_anova)
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
shapiro.test(spon_anova$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  spon_anova$residuals
    ## W = 0.95688, p-value = 0.09945

``` r
par(mfrow=c(2,2))
plot(spon_anova)
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
#normal and homogeneity pass


#homogeneity of variances
leveneTest(turning_rate ~ size, data = spon_turn)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  1  0.0169 0.8972
    ##       42

``` r
t.test(turning_rate ~ size, data = spon_turn)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  turning_rate by size
    ## t = -0.58589, df = 34.338, p-value = 0.5618
    ## alternative hypothesis: true difference in means between group large and group small is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08596025  0.04747703
    ## sample estimates:
    ## mean in group large mean in group small 
    ##           0.2243052           0.2435468

``` r
avg_turnrate <- mean(spon_turn$turning_rate)
print(avg_turnrate)
```

    ## [1] 0.2321768

``` r
#avg turn rate is .2321768
```

``` r
#calculate turning duration on all data
# Calculate density 

cym_dat_frames <- read.csv("data/rawdata_allcombinedSep28_start_end.csv")
cym_dat_frames <- cym_dat_frames %>% 
  mutate(turning_duration = (frame_end - frame_st)/240 * 1000)

#add turning duration to cym_dat df
cym_dat$turning_duration <- cym_dat_frames$turning_duration

#calculate turning rate
cym_dat <- cym_dat %>% 
  mutate(turning_rate = turning_angle_absolute_value / turning_duration)

#categorize responses

cym_dat$response_type <- ifelse(
  (cym_dat$size == "large" & cym_dat$turning_rate > 0.22) |
  (cym_dat$size == "small" & cym_dat$turning_rate > 0.24),
  "fast",
  "slow"
)

#make sure variables are as they are supposed to be

cym_dat$s_w_reactor <- as.factor(cym_dat$s_w_reactor)
cym_dat$size <- as.factor(cym_dat$size)
cym_dat$school <- as.factor(cym_dat$school)
cym_dat$stimulus <- as.factor(cym_dat$stimulus)
cym_dat$response_type <- as.factor(cym_dat$response_type)
```

``` r
#visualize data fast and slow responses

#all together
ggplot(cym_dat,aes(x=turning_duration,y=turning_angle_absolute_value,color=size))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=size))+
        scale_color_manual(name='Size',
                     breaks=c('large', 'small'),
                     values=c('large'='#E69F00', 'small'='#0C7BDC'))+
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#visualize percent slow
summary_table <- cym_dat %>%
  group_by(size, response_type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
```

    ## `summarise()` has grouped output by 'size'. You can override using the
    ## `.groups` argument.

``` r
ggplot(summary_table, aes(x = size, y = percentage, fill = response_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Fast and Slow Responses by Size Group", y = "Percentage", x = "Size") +
  scale_fill_fish_d(option = "Trimma_lantana", name = "Response Type") +
  theme_minimal()
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
#frequency of fast and slow
summary_table <- cym_dat %>%
  group_by(size, response_type) %>%
  summarise(count = n())
```

    ## `summarise()` has grouped output by 'size'. You can override using the
    ## `.groups` argument.

``` r
ggplot(summary_table, aes(x = size, y = count, fill = response_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Fast and Slow Responses by Size Group", y = "Frequency") +
  scale_fill_manual(values = c("gray40", "gray80"), name="Response Type") +
  theme_minimal()
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
#scatter plot grouped by fast and slow

ggplot(cym_dat,aes(x=turning_duration,y=turning_angle_absolute_value,color=response_type))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=response_type))+
        scale_color_manual(name='Response Type',
                     breaks=c('fast', 'slow'),
                     values=c('large'='#E69F00', 'slow'='#0C7BDC'))+
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
# Calculating NND for each responder within the same school and stimulus group
#NAs come from two having the same nearest neighbor distance

nnd_df$school <- as.factor(nnd_df$school)
nnd_df$size <- as.factor(nnd_df$size)
nnd_df$stimulus <- as.factor(nnd_df$stimulus)

nnd_df <- nnd_df %>%
  group_by(school, stimulus) %>%
  mutate(
    NND = sqrt((s_head_x - lag(s_head_x))^2 + (s_head_y - lag(s_head_y))^2)
  ) %>%
  ungroup()

#nnd from head to head using Euclidean distance formula
nnd_df <- nnd_df %>%
  group_by(school, size, stimulus) %>%
  mutate(
    NND = sqrt((s_head_x - lag(s_head_x))^2 + (s_head_y - lag(s_head_y))^2)
  ) %>%
  ungroup()

#If we want:
# Replace NA values in the NND column with 0 
#nnd_df$NND[is.na(nnd_df$NND)] <- 0

cym_dat$NND <- nnd_df$NND

#add nnd to wave df
nnd_wave_dat <- merge(wave_dat, nnd_df[, c('school', 'stimulus', 'responder', 'NND')], 
                         by = c('school', 'stimulus', 'responder'), 
                         all.x = TRUE)

#average NND 
avgNND_result <- aggregate(NND ~ size, data = cym_dat, FUN = function(x) mean(x, na.rm = TRUE))
print(avgNND_result)
```

    ##    size      NND
    ## 1 large 19.64807
    ## 2 small 13.02017

``` r
manwhit_NND <- wilcox.test(NND ~ size, data = cym_dat)
print(manwhit_NND) 
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  NND by size
    ## W = 30968, p-value = 2.275e-10
    ## alternative hypothesis: true location shift is not equal to 0

``` r
#they are significantly different from each other and further apart!  

boxplot(NND ~ size, data = cym_dat,
        xlab = "Size", ylab = "Distance (cm)", col = c("gray40", "gray80"))
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#ANCOVA
ancova_mod <- aov(turning_rate ~ size + response_type, data = cym_dat)
summary(ancova_mod)
```

    ##                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## size            1   16.4   16.42    6.93  0.00877 ** 
    ## response_type   1  109.7  109.68   46.28 3.27e-11 ***
    ## Residuals     450 1066.4    2.37                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
par(mfrow=c(2,2))
plot(ancova_mod)
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
wilcox.test(turning_rate ~ response_type, data = cym_dat)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  turning_rate by response_type
    ## W = 28341, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

``` r
# p-value <2.2e-16, conclusion: there is a statistically significant difference in the distributions of "turning_rate" between the two response types.
```

``` r
#stuff to come back to in order to filter out longer latencies that should not be considered part of the wave

#quartiles <- wave_dat_fast %>%
 # group_by(size) %>%
#  summarize(
 #   Q1 = quantile(latency_ms, 0.25),
  #  Median = median(latency_ms),
  #  Q3 = quantile(latency_ms, 0.75)
#  )

# View the quartiles for each size class
#print(quartiles)

#filtered_wave_dat_fast <- wave_dat_fast %>%
#  group_by(size) %>%
#  filter(latency_ms <= (quantile(latency_ms, 0.75) + 1.5 * IQR(latency_ms))) %>%
#  ungroup()

#ggplot(filtered_wave_dat_fast) +
#  geom_point(aes(x = latency_ms, y = turning_rate, color = size)) +
#  geom_smooth(aes(x = latency_ms, y = turning_rate, color = size), method = "lm") +
#  scale_color_fish_d(option = "Trimma_lantana") 
```

``` r
#Only interested in fast responses, making new dataset excluding slow responses

fast_dat <- cym_dat %>%
  filter(response_type == "fast")

#glm for fast
#mod_fast <- glm(latency_ms ~ s_w_reactor + distance_from_stimulus + size+ angle_between_fish_and_stimulus + distance_from_first_responder +NND, data = fast_dat, family=poisson, na.action = na.exclude)
#summary(mod_fast)
#par(mfrow=c(2,2))
#plot(mod_fast)

#subsetting just the wave
wave_dat_fast <- subset(fast_dat, substr(responder, 1, 1) == "w")


#frequency of latency
histogram <- hist(wave_dat_fast$latency_ms, 
                  main = "Frequency Histogram of Latency (ms)",
                  xlab = "Latency (ms)",
                  ylab = "Frequency",
                  col = "lightblue",  # Adjust color as needed
                  border = "black",  # Adjust border color as needed
                  breaks = 20)  # You can adjust the number of bins by changing 'breaks'
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Plot the histogram
par(mfrow=c(1,1))
plot(histogram, col = "lightblue", border = "black")
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
ggplot(wave_dat_fast) +
  geom_point(aes(x = latency_ms, y = turning_rate, color = size)) +
  geom_smooth(aes(x = latency_ms, y = turning_rate, color = size), method = "lm") +
  scale_color_fish_d(option = "Trimma_lantana") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
latency_df <- wave_dat_fast[c("school", "stimulus", "size", "responder", "latency_ms", "turning_rate")]




#glm for wave
glm_wave_fast <- glm(latency_ms ~ size+ angle_between_fish_and_stimulus + distance_from_first_responder + NND,data = wave_dat_fast, family=Gamma(link = "log"), na.action = na.exclude)
summary(glm_wave_fast)
```

    ## 
    ## Call:
    ## glm(formula = latency_ms ~ size + angle_between_fish_and_stimulus + 
    ##     distance_from_first_responder + NND, family = Gamma(link = "log"), 
    ##     data = wave_dat_fast, na.action = na.exclude)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3409  -0.4967  -0.1336   0.2082   1.6107  
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      5.4347868  0.1305232  41.638  < 2e-16 ***
    ## sizesmall                        0.4656562  0.0901161   5.167 5.64e-07 ***
    ## angle_between_fish_and_stimulus -0.0008496  0.0006864  -1.238  0.21723    
    ## distance_from_first_responder    0.0085481  0.0031592   2.706  0.00739 ** 
    ## NND                             -0.0016675  0.0041097  -0.406  0.68536    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.3461998)
    ## 
    ##     Null deviance: 75.546  on 208  degrees of freedom
    ## Residual deviance: 62.990  on 204  degrees of freedom
    ## AIC: 2726.9
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
par(mfrow=c(2,2))
plot(glm_wave_fast)
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
with(summary(glm_wave_fast), 1 - deviance/null.deviance)
```

    ## [1] 0.1661971

``` r
#taking out all insignificant variables
glm_size_distfr <- glm(latency_ms ~ size + distance_from_first_responder +NND, data = wave_dat_fast, family=Gamma(link = "log"))
summary(glm_size_distfr)
```

    ## 
    ## Call:
    ## glm(formula = latency_ms ~ size + distance_from_first_responder + 
    ##     NND, family = Gamma(link = "log"), data = wave_dat_fast)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3535  -0.4999  -0.1056   0.2059   1.5251  
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    5.406899   0.127851  42.291  < 2e-16 ***
    ## sizesmall                      0.470122   0.090039   5.221 4.35e-07 ***
    ## distance_from_first_responder  0.007475   0.003035   2.463   0.0146 *  
    ## NND                           -0.001823   0.004109  -0.444   0.6579    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.3461796)
    ## 
    ##     Null deviance: 75.546  on 208  degrees of freedom
    ## Residual deviance: 63.523  on 205  degrees of freedom
    ## AIC: 2726.8
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
par(mfrow=c(2,2))
plot(glm_size_distfr)
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
#calculate McFadden's R-squared for model for size, distance, NND from first responder glm
with(summary(glm_size_distfr), 1 - deviance/null.deviance)
```

    ## [1] 0.1591377

``` r
glm_size <- glm(latency_ms ~ size, data = wave_dat_fast, family=Gamma(link = "log"))
summary(glm_size)
```

    ## 
    ## Call:
    ## glm(formula = latency_ms ~ size, family = Gamma(link = "log"), 
    ##     data = wave_dat_fast)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3583  -0.4910  -0.1352   0.2313   1.8493  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.57289    0.06235  89.384  < 2e-16 ***
    ## sizesmall    0.43549    0.08479   5.136 6.46e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.3731722)
    ## 
    ##     Null deviance: 75.546  on 208  degrees of freedom
    ## Residual deviance: 65.891  on 207  degrees of freedom
    ## AIC: 2730.8
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
par(mfrow=c(2,2))
plot(glm_size)
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

``` r
with(summary(glm_size), 1 - deviance/null.deviance)
```

    ## [1] 0.1277931

``` r
#linear model for just the wave
lm_wavedat_fast <- lm(latency_ms ~ size, data=wave_dat_fast)
summary(lm_wavedat_fast)
```

    ## 
    ## Call:
    ## lm(formula = latency_ms ~ size, data = wave_dat_fast)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -336.02 -125.69  -38.19   82.61 1272.38 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   263.19      23.46  11.220  < 2e-16 ***
    ## sizesmall     143.63      31.90   4.502 1.12e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 229.8 on 207 degrees of freedom
    ## Multiple R-squared:  0.08918,    Adjusted R-squared:  0.08478 
    ## F-statistic: 20.27 on 1 and 207 DF,  p-value: 1.123e-05

``` r
#normality
shapiro.test(lm_wavedat_fast$residuals) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  lm_wavedat_fast$residuals
    ## W = 0.81504, p-value = 5.023e-15

``` r
#homogeneity of variances
bartlett.test(latency_ms ~ size, data=wave_dat_fast)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  latency_ms by size
    ## Bartlett's K-squared = 71.483, df = 1, p-value < 2.2e-16

``` r
wilcox.test(latency_ms ~ size, data = wave_dat_fast)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  latency_ms by size
    ## W = 3683.5, p-value = 6.5e-05
    ## alternative hypothesis: true location shift is not equal to 0

``` r
#for later when we have all the schools done, this is a multi-comparison to see 
#if the schools were sig different
aov_school <- aov(latency_ms ~school * size, data=wave_dat_fast)
summary(aov_school)
```

    ##              Df  Sum Sq Mean Sq F value   Pr(>F)    
    ## school        2 1281023  640512  13.407 3.40e-06 ***
    ## size          1  953245  953245  19.952 1.32e-05 ***
    ## school:size   2   73446   36723   0.769    0.465    
    ## Residuals   203 9698543   47776                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(aov_school, which='school:size')
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = latency_ms ~ school * size, data = wave_dat_fast)
    ## 
    ## $`school:size`
    ##                      diff         lwr      upr     p adj
    ## 2:large-1:large  70.61611  -79.382822 220.6150 0.7539830
    ## 3:large-1:large 141.51088  -26.353566 309.3753 0.1524229
    ## 1:small-1:large  97.92263  -58.101219 253.9465 0.4643748
    ## 2:small-1:large 196.00992   51.246220 340.7736 0.0018280
    ## 3:small-1:large 334.94393  180.053786 489.8341 0.0000000
    ## 3:large-2:large  70.89477  -90.226900 232.0164 0.8031414
    ## 1:small-2:large  27.30653 -121.438772 176.0518 0.9949897
    ## 2:small-2:large 125.39381  -11.493916 262.2815 0.0935020
    ## 3:small-2:large 264.32783  116.772144 411.8835 0.0000089
    ## 1:small-3:large -43.58824 -210.333425 123.1569 0.9749531
    ## 2:small-3:large  54.49904 -101.760492 210.7586 0.9163762
    ## 3:small-3:large 193.43306   27.748202 359.1179 0.0118674
    ## 2:small-1:small  98.08729  -45.377044 241.5516 0.3649276
    ## 3:small-1:small 237.02130   83.344877 390.6977 0.0002142
    ## 3:small-2:small 138.93402   -3.296536 281.1646 0.0598263

``` r
sch_size_multicomp <- HSD.test(aov_school, trt = c("school", "size"), console = TRUE)
```

    ## 
    ## Study: aov_school ~ c("school", "size")
    ## 
    ## HSD Test for latency_ms 
    ## 
    ## Mean Square Error:  47776.07 
    ## 
    ## school:size,  means
    ## 
    ##         latency_ms       std  r   Min    Max
    ## 1:large   197.6531  89.99878 32  66.7  475.0
    ## 1:small   295.5758 201.63413 33  70.8  987.5
    ## 2:large   268.2692 111.71392 39 112.5  625.0
    ## 2:small   393.6630 233.11708 46  91.7 1200.0
    ## 3:large   339.1640 115.99488 25 150.0  645.8
    ## 3:small   532.5971 385.48794 34 158.3 1679.2
    ## 
    ## Alpha: 0.05 ; DF Error: 203 
    ## Critical Value of Studentized Range: 4.068891 
    ## 
    ## Groups according to probability of means differences and alpha level( 0.05 )
    ## 
    ## Treatments with the same letter are not significantly different.
    ## 
    ##         latency_ms groups
    ## 3:small   532.5971      a
    ## 2:small   393.6630     ab
    ## 3:large   339.1640     bc
    ## 1:small   295.5758     bc
    ## 2:large   268.2692     bc
    ## 1:large   197.6531      c

``` r
bar.group(sch_size_multicomp$groups, ylim=c(0,650))

sch_size_multicomp <- HSD.test(aov_school, trt = c("school", "size"), console = TRUE)
```

    ## 
    ## Study: aov_school ~ c("school", "size")
    ## 
    ## HSD Test for latency_ms 
    ## 
    ## Mean Square Error:  47776.07 
    ## 
    ## school:size,  means
    ## 
    ##         latency_ms       std  r   Min    Max
    ## 1:large   197.6531  89.99878 32  66.7  475.0
    ## 1:small   295.5758 201.63413 33  70.8  987.5
    ## 2:large   268.2692 111.71392 39 112.5  625.0
    ## 2:small   393.6630 233.11708 46  91.7 1200.0
    ## 3:large   339.1640 115.99488 25 150.0  645.8
    ## 3:small   532.5971 385.48794 34 158.3 1679.2
    ## 
    ## Alpha: 0.05 ; DF Error: 203 
    ## Critical Value of Studentized Range: 4.068891 
    ## 
    ## Groups according to probability of means differences and alpha level( 0.05 )
    ## 
    ## Treatments with the same letter are not significantly different.
    ## 
    ##         latency_ms groups
    ## 3:small   532.5971      a
    ## 2:small   393.6630     ab
    ## 3:large   339.1640     bc
    ## 1:small   295.5758     bc
    ## 2:large   268.2692     bc
    ## 1:large   197.6531      c

``` r
bar.group(sch_size_multicomp$groups, ylim=c(0,650))

#just small

small_data <- wave_dat_fast %>% filter(size == "small")

aov_small <- aov(latency_ms ~school, data=small_data)
summary(aov_small)
```

    ##              Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## school        2  954223  477111   6.067 0.00317 **
    ## Residuals   110 8650294   78639                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
small_size_multicomp <- HSD.test(aov_small, trt = c("school"), console = TRUE)
```

    ## 
    ## Study: aov_small ~ c("school")
    ## 
    ## HSD Test for latency_ms 
    ## 
    ## Mean Square Error:  78639.04 
    ## 
    ## school,  means
    ## 
    ##   latency_ms      std  r   Min    Max
    ## 1   295.5758 201.6341 33  70.8  987.5
    ## 2   393.6630 233.1171 46  91.7 1200.0
    ## 3   532.5971 385.4879 34 158.3 1679.2
    ## 
    ## Alpha: 0.05 ; DF Error: 110 
    ## Critical Value of Studentized Range: 3.359968 
    ## 
    ## Groups according to probability of means differences and alpha level( 0.05 )
    ## 
    ## Treatments with the same letter are not significantly different.
    ## 
    ##   latency_ms groups
    ## 3   532.5971      a
    ## 2   393.6630     ab
    ## 1   295.5758      b

``` r
bar.group(small_size_multicomp$groups, ylim=c(0,650))

#just large

large_data <- wave_dat_fast %>% filter(size == "large")

aov_large <- aov(latency_ms ~school, data=large_data)
summary(aov_large)
```

    ##             Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## school       2  282750  141375   12.54 1.5e-05 ***
    ## Residuals   93 1048249   11271                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
large_size_multicomp <- HSD.test(aov_large, trt = c("school"), console = TRUE)
```

    ## 
    ## Study: aov_large ~ c("school")
    ## 
    ## HSD Test for latency_ms 
    ## 
    ## Mean Square Error:  11271.49 
    ## 
    ## school,  means
    ## 
    ##   latency_ms       std  r   Min   Max
    ## 1   197.6531  89.99878 32  66.7 475.0
    ## 2   268.2692 111.71392 39 112.5 625.0
    ## 3   339.1640 115.99488 25 150.0 645.8
    ## 
    ## Alpha: 0.05 ; DF Error: 93 
    ## Critical Value of Studentized Range: 3.368392 
    ## 
    ## Groups according to probability of means differences and alpha level( 0.05 )
    ## 
    ## Treatments with the same letter are not significantly different.
    ## 
    ##   latency_ms groups
    ## 3   339.1640      a
    ## 2   268.2692      b
    ## 1   197.6531      c

``` r
bar.group(large_size_multicomp$groups, ylim=c(0,650))
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

``` r
#Plots


#Latency vs distance from first responder
ggplot(wave_dat_fast,aes(x=distance_from_first_responder,y=latency_ms,color=size))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=size))+
        scale_color_fish_d(option = "Trimma_lantana", name="Size") +
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
  labs(x = "Distance from First Responder",
       y = "Latency (ms)")+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#Latency vs NND
ggplot(wave_dat_fast,aes(x=NND,y=latency_ms,color=size))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=size))+
        scale_color_fish_d(option = "Trimma_lantana", name="Size") +
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
  labs(x = "NND",
       y = "Latency (ms)")+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
#turning rate for all by responder type
ggplot(fast_dat,aes(x=NND,y=turning_rate,color=s_w_reactor))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=s_w_reactor))+
        scale_color_fish_d(option = "Trimma_lantana", name="Reactor") +
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
  labs(x = "NND",
       y = "Turning rate (degree/sec)")+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 24 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 24 rows containing missing values (`geom_point()`).

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
ggplot(fast_dat,aes(x=NND,y=turning_rate,color=size))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=size))+
        scale_color_fish_d(option = "Trimma_lantana", name="Size") +
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
  labs(x = "NND",
       y = "Turning rate (degree/sec)")+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 24 rows containing non-finite values (`stat_smooth()`).
    ## Removed 24 rows containing missing values (`geom_point()`).

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
#Correlation and Principal Component Analysis, not using this


#new df with specified columns
wave_fast_forPCA <- wave_dat_fast[, c("latency_ms", "s_w_reactor", "distance_from_stimulus", "size", "angle_between_fish_and_stimulus", "distance_from_first_responder","NND")]


#make small 0 and large 1
wave_fast_forPCA <- wave_fast_forPCA %>%
  mutate(size_ID = ifelse(size == "small", 0, 1))



#use sapply to make wave_dat_fast numeric

wave_fast_new <- wave_fast_forPCA[sapply(wave_fast_forPCA, is.numeric)]

#correlation
wave_corr <- sapply(wave_fast_new[, setdiff(names(wave_fast_new), "latency_ms")], function(x) {
  cor(wave_fast_new$latency_ms, x, method = "pearson")
})
print(wave_corr)
```

    ##          distance_from_stimulus angle_between_fish_and_stimulus 
    ##                      0.05041053                     -0.07227380 
    ##   distance_from_first_responder                             NND 
    ##                      0.07761140                     -0.11248255 
    ##                         size_ID 
    ##                     -0.29863354

``` r
#quick visualization 
ggplot(wave_dat_fast) +
  geom_point(aes(x = distance_from_first_responder, y = latency_ms, color = size)) +
  geom_smooth(aes(x = distance_from_first_responder, y = latency_ms, color = size), method = "lm") +
  scale_color_fish_d(option = "Trimma_lantana") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot(cym_dat,aes(x=turning_duration,y=turning_angle_absolute_value,color=response_type))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=response_type))+
        scale_color_manual(name='Response Type',
                     breaks=c('fast', 'slow'),
                     values=c('large'='#E69F00', 'slow'='#0C7BDC'))+
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# Predict the values using the fitted model
predicted_values <- predict(glm_size_distfr, type = "response")

# Create a data frame for plotting
plot_data <- data.frame(Observed = wave_dat_fast$latency_ms, Predicted = predicted_values)

#scatterplot
ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a reference line
  labs(x = "Observed", y = "Predicted") +
  theme_minimal()
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplt <- ggplot(wave_dat,aes(x=distance_from_first_responder,y=latency_ms,color=size))+
         geom_point()+
         theme_classic()

ggplt+geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=size))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
ggplt+scale_color_manual(values=c("#E69F00","#0C7BDC"))
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
ggplt+scale_color_manual(name='Size',
                     breaks=c('large', 'small'),
                     values=c('large'='#E69F00', 'small'='#0C7BDC'))+
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))
```

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
#plots 

ggplot(wave_dat_fast,aes(x=distance_from_first_responder,y=latency_ms,color=size))+
         geom_point()+
         geom_smooth(method=lm,fullrange=TRUE,
                  aes(color=size))+
        scale_color_manual(name='Size',
                     breaks=c('large', 'small'),
                     values=c('large'='#E69F00', 'small'='#0C7BDC'))+
   theme(legend.title=element_text(size=14),
       legend.text=element_text(size=14))+
         theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](fhlcode_draft1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
