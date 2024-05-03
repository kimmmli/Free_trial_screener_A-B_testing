
# 1. Case overview

**Summary**: Udacity currently have two options on the course overview page: **“start free trial”**, and **“access course materials”**.

- If the student clicks **“start free trial”**, they will be asked to enter their credit card information, and then they will be enrolled in a free trial for the paid version of the course. After 14 days, they will automatically be charged unless they cancel first.
- If the student clicks **“access course materials”**, they will be able to view the videos and take the quizzes for free, but they will not receive coaching support or a verified certificate, and they will not submit their final project for feedback.

**Experiment**: Free Trial Screener

**Goal**: Maximize the course completion rate of **“Free Trial”**users through guiding the students who do not have enough time to **“access course materials”**.

**Experiment Hypothesis**: The hypothesis was that this might set clearer expectations for students upfront, thus reducing the number of frustrated students who left the free trial because they didn’t have enough time—without significantly reducing the number of students to continue past the free trial and eventually complete the course. If this hypothesis held true, Udacity could improve the overall student experience and improve coaches’ capacity to support students who are likely to complete the course.

**Experiment Change**: For the users who click on **“start free trial”**, Udacity will ask the users how much time they are available to devote to the course.

- For users who will devote 5 or more hours per week. It’s the same as usual.
- For users who will devote Less than 5 hours per week, Udacity will suggest the users choose “access course materials”

**Unit of diversion**: cookies

- If the student enrolls in the free trial, they are tracked by user-id from that point forward. **The same user-id cannot enroll in the free trial twice. For users that do not enroll, their user-id is not tracked in the experiment, even if they were signed in when they visited the course overview page.**

# 2. Customer Funnel

Unique Cookie (overview) -> Unique Cookie(click free trial) -> Enrollments -> Payments

# 3. Experiment Design

## 3.1 Metric Choice

**Invariant metrics:** Number of cookies, number of clicks, click-through-probability

According to the experiment design, the unit of diversion is a cookie, which means it will be randomly distributed across experiment and comparison group. In addition, both number of clicks and click-through-probability happen before the pop-up schedule checking screener so both of them will be invariant in ideal cases.

**Evaluation metrics:** Gross conversion, Retention, Net conversion

All of the three metrics can be affected by the pop-up schedule checking screener so both of them will be variant in ideal cases.

## 3.2 Measuring Standard deviation/ Variability

**Q: For each metric you selected as an evaluation metric, make an analytic estimate of its standard error, given a sample size of 5,000 cookies visiting the course overview page. Enter each estimate in the appropriate box to 4 decimal places**

Baseline dataset:

| Unique cookies to view course overview page per day: | 40000 |
| --- | --- |
| Unique cookies to click "Start free trial" per day: | 3200 |
| Enrollments per day: | 660 |
| Click-through-probability on "Start free trial": | 0.08 |
| Probability of enrolling, given click: | 0.20625 |
| Probability of payment, given enroll: | 0.53 |
| Probability of payment, given click | 0.1093125 |

According to Central Limit Theorem, when a sample size is large enough, we can assume the sample mean is normally distributed. Knowing normal distribution, we can analytically calculate the three metrics’ variance and standard error.


variance = p*(1-p) \
SE = variance/sqrt(n)


|  | Standard Error |
| --- | --- |
| Gross conversion | 0.0202 |
| Retention | 0.0549 |
| Net conversion | 0.0156 |

** **Please pay attention to the n in the denominator** **

** **n for different metric is different, it is the value of the denominator in the metric definition equation****


The above approach is using analytical solution, however, empirical way (bootstrap) seems to be more robust. In this case, we are not provided with data to do bootstrap. Normally, we should do both.

## 3.3 Sample Size

**Q: Choosing Number of Samples given Power**

Using the analytic estimates of variance, how many pageviews total (across both groups) would you need to collect to adequately power the experiment? Use an alpha of 0.05 and a beta of 0.2. Make sure you have enough power for each metric.

**A:**

I did not adjust significant level based on the fact that we have multiple metrics. Bonferroni is one approach to such adjustment, and there are more approaches to do so. I did not adjust, because these metrics are closely related to each other, and Bonferroni would be too conservative, cause inflamation of the sample sizes.

### 3.3.1 Web-based Formula to calculate sample size

Using the below page https://www.evanmiller.org/ab-testing/sample-size.html, enter baseline conversion rate, minimum detectable effect, statistical power 1-beta (the probability of correctly rejecting the null hypothesis when it's false; true positive) , significance level alpha (type 1 error; a true null hypothesis is incorrectly rejected; false positive), we can calculate the sample size for one group. 

We need to double to get the size for both experiment and control group.

| Metric | Sample Size | Both group sample size | Unique cookie num |
| --- | --- | --- | --- |
| Net Conversion | 27,413 | 54826 | 54826/0.08 = 685325 |
| Retention | 39,115 | 78230 | 78230/0.20625/0.08=4741212 |
| Gross Conversion | 25,835 | 51670 | 51670/0.08 = 645875 |

### 3.3.2 Empirical way to calculate sample size


```r
## Strategy: For a bunch of Ns, compute the z_star by achieving desired alpha, then## compute what beta would be for that N using the acquired z_star.## Pick the smallest N at which beta crosses the desired value# Inputs:#   The desired alpha for a two-tailed test# Returns: The z-critical valueget_z_star =function(alpha) {
return(-qnorm(alpha / 2))
}

alpha <- 0.05
retention_z_star <- get_z_star(alpha = alpha)
net_conversion_z_star <- get_z_star(alpha = alpha)
gross_conversion_z_star <- get_z_star(alpha = alpha)

# Inputs:#   z-star: The z-critical value#   s: The standard error of the metric at N=1#   d_min: The practical significance level#   N: The sample size of each group of the experiment# Returns: The beta value of the two-tailed testget_beta =function(z_star, s, d_min, N) {
    SE = s /  sqrt(N)
return(pnorm(z_star * SE, mean=d_min, sd=SE))
}

# Inputs:#   s: The standard error of the metric with N=1 in each group#   d_min: The practical significance level#   Ns: The sample sizes to try#   alpha: The desired alpha level of the test#   beta: The desired beta level of the test# Returns: The smallest N out of the given Ns that will achieve the desired#          beta. There should be at least N samples in each group of the experiment.#          If none of the given Ns will work, returns -1. N is the number of#          samples in each group.required_size =function(s, d_min, Ns=1:500000, alpha=0.05, beta=0.2) {
for (Nin Ns) {
if (get_beta(get_z_star(alpha), s, d_min, N) <= beta) {
return(N)
        }
    }

return(-1)
}

#for retentiond_min_retention <- 0.01
p_payment_enroll <- 0.53
n <- 1
SE_retention <- sqrt(p_payment_enroll*(1-p_payment_enroll)/n)
retention_req_size <- required_size(s = SE_retention,
                                    d_min = d_min_retention,
                                    alpha = alpha,
                                    beta = 0.2)
retention_req_size
```

```
## [1] 19552
```

```r
# For net conversiond_min_net_conversion <- 0.0075
p_payment_click <- 0.1093125
n <- 1
SE_net_conversion <- sqrt(p_payment_click*(1-p_payment_click)/n)
net_conversion_req_size <- required_size(s = SE_net_conversion,
                                    d_min = d_min_net_conversion,
                                    alpha = alpha,
                                    beta = 0.2)
net_conversion_req_size
```

```
## [1] 13586
```

```r
# For gross conversiond_min_gross_conversion <- 0.01
p_enroll_click <- 0.20625
n <- 1
SE_gross_conversion <- sqrt(p_enroll_click*(1-p_enroll_click)/n)
gross_conversion_req_size <- required_size(s = SE_gross_conversion,
                                    d_min = d_min_gross_conversion,
                                    alpha = alpha,
                                    beta = 0.2)
gross_conversion_req_size
```

```
## [1] 12850
```

## 3.4 Duration and Traffic

**Q: What percentage of Udacity’s traffic would you divert to this experiment (assuming there were no other experiments you wanted to run simultaneously)? Is the change risky enough that you wouldn’t want to run on all traffic?**

**Q: Given the percentage you chose, how long would the experiment take to run, using the analytic estimates of variance? If the answer is longer than a few weeks, then this is unreasonably long, and you should reconsider an earlier decision.**

Using above results and Udacity currently has 40,000 pageviews (unique cookies) to view course overview page per day.

| Metric Name | Sample Size | Minimum pageviews | Fraction of experiment traffic | Duration |
| --- | --- | --- | --- | --- |
| 0 | Gross conversion | 25,835 | 645,875 | 17 |
| 1 | Retention | 39,115 | 4,741,212 | 119 |
| 2 | Net conversion | 27,413 | 685,325 | 18 |

We need to reduce this duration, considering the fact that we do not want to guide 100% of the traffic to the experiment. The reason is, the experiment may cause some unexpected side-effects, so it is better not to expose all traffic to it.

We can loosen the power of the test, and increase the alpha for the retention, since this metric is our bottleneck. Or increase the practical significance to 2%, instead of the default 1%. Then the total unique cookies required would be 1,185,455, and the minimum days would be reduced to 30.

Increasing the practical significance means that even though the change in the metric is statistically significant and practically significant based on the old criterion, now we deem it as insignificant. A good decision?

To me it is worth trying, but [the provided data about the traffic](https://docs.google.com/spreadsheets/d/1Mu5u9GrybDdska-ljPXyBjTpdZIUev_6i7t4LRDfXM8/edit#gid=0) is insufficient for keeping the retention. So we drop the retention, and continue with the net and gross conversion metrics.

If we **drop the retention metric**, then the net conversion would rule the sample-sizing, and the experiment needs at least 18 days. With 60% traffic, it would be 29 days. (Btw, the platform of this project at Udacity is disappointingly weak, it does not accept 685,326 as the required overview page, but it accepts 685,325 ).

# 4. Analysis

Experiment data Control group:

| Date | Pageviews | Clicks | Enrollments | Payments |
| --- | --- | --- | --- | --- |
| Sat, Oct 11 | 7723 | 687 | 134 | 70 |
| Sun, Oct 12 | 9102 | 779 | 147 | 70 |
| Mon, Oct 13 | 10511 | 909 | 167 | 95 |
| Tue, Oct 14 | 9871 | 836 | 156 | 105 |
| Wed, Oct 15 | 10014 | 837 | 163 | 64 |
| Thu, Oct 16 | 9670 | 823 | 138 | 82 |
| Fri, Oct 17 | 9008 | 748 | 146 | 76 |
| Sat, Oct 18 | 7434 | 632 | 110 | 70 |
| Sun, Oct 19 | 8459 | 691 | 131 | 60 |
| Mon, Oct 20 | 10667 | 861 | 165 | 97 |
| Tue, Oct 21 | 10660 | 867 | 196 | 105 |
| Wed, Oct 22 | 9947 | 838 | 162 | 92 |
| Thu, Oct 23 | 8324 | 665 | 127 | 56 |
| Fri, Oct 24 | 9434 | 673 | 220 | 122 |
| Sat, Oct 25 | 8687 | 691 | 176 | 128 |
| Sun, Oct 26 | 8896 | 708 | 161 | 104 |
| Mon, Oct 27 | 9535 | 759 | 233 | 124 |
| Tue, Oct 28 | 9363 | 736 | 154 | 91 |
| Wed, Oct 29 | 9327 | 739 | 196 | 86 |
| Thu, Oct 30 | 9345 | 734 | 167 | 75 |
| Fri, Oct 31 | 8890 | 706 | 174 | 101 |
| Sat, Nov 1 | 8460 | 681 | 156 | 93 |
| Sun, Nov 2 | 8836 | 693 | 206 | 67 |
| Mon, Nov 3 | 9437 | 788 |  |  |
| Tue, Nov 4 | 9420 | 781 |  |  |
| Wed, Nov 5 | 9570 | 805 |  |  |
| Thu, Nov 6 | 9921 | 830 |  |  |
| Fri, Nov 7 | 9424 | 781 |  |  |
| Sat, Nov 8 | 9010 | 756 |  |  |
| Sun, Nov 9 | 9656 | 825 |  |  |
| Mon, Nov 10 | 10419 | 874 |  |  |
| Tue, Nov 11 | 9880 | 830 |  |  |
| Wed, Nov 12 | 10134 | 801 |  |  |
| Thu, Nov 13 | 9717 | 814 |  |  |
| Fri, Nov 14 | 9192 | 735 |  |  |
| Sat, Nov 15 | 8630 | 743 |  |  |
| Sun, Nov 16 | 8970 | 722 |  |  |

Experiment data Experiment group:

| Sat, Oct 11 | 7716 | 686 | 105 | 34 |
| --- | --- | --- | --- | --- |
| Sun, Oct 12 | 9288 | 785 | 116 | 91 |
| Mon, Oct 13 | 10480 | 884 | 145 | 79 |
| Tue, Oct 14 | 9867 | 827 | 138 | 92 |
| Wed, Oct 15 | 9793 | 832 | 140 | 94 |
| Thu, Oct 16 | 9500 | 788 | 129 | 61 |
| Fri, Oct 17 | 9088 | 780 | 127 | 44 |
| Sat, Oct 18 | 7664 | 652 | 94 | 62 |
| Sun, Oct 19 | 8434 | 697 | 120 | 77 |
| Mon, Oct 20 | 10496 | 860 | 153 | 98 |
| Tue, Oct 21 | 10551 | 864 | 143 | 71 |
| Wed, Oct 22 | 9737 | 801 | 128 | 70 |
| Thu, Oct 23 | 8176 | 642 | 122 | 68 |
| Fri, Oct 24 | 9402 | 697 | 194 | 94 |
| Sat, Oct 25 | 8669 | 669 | 127 | 81 |
| Sun, Oct 26 | 8881 | 693 | 153 | 101 |
| Mon, Oct 27 | 9655 | 771 | 213 | 119 |
| Tue, Oct 28 | 9396 | 736 | 162 | 120 |
| Wed, Oct 29 | 9262 | 727 | 201 | 96 |
| Thu, Oct 30 | 9308 | 728 | 207 | 67 |
| Fri, Oct 31 | 8715 | 722 | 182 | 123 |
| Sat, Nov 1 | 8448 | 695 | 142 | 100 |
| Sun, Nov 2 | 8836 | 724 | 182 | 103 |
| Mon, Nov 3 | 9359 | 789 |  |  |
| Tue, Nov 4 | 9427 | 743 |  |  |
| Wed, Nov 5 | 9633 | 808 |  |  |
| Thu, Nov 6 | 9842 | 831 |  |  |
| Fri, Nov 7 | 9272 | 767 |  |  |
| Sat, Nov 8 | 8969 | 760 |  |  |
| Sun, Nov 9 | 9697 | 850 |  |  |
| Mon, Nov 10 | 10445 | 851 |  |  |
| Tue, Nov 11 | 9931 | 831 |  |  |
| Wed, Nov 12 | 10042 | 802 |  |  |
| Thu, Nov 13 | 9721 | 829 |  |  |
| Fri, Nov 14 | 9304 | 770 |  |  |
| Sat, Nov 15 | 8668 | 724 |  |  |
| Sun, Nov 16 | 8988 | 710 |  |  |


## 4.1 Sanity check

First we need to check whether the invariance variable remains the same.

### 4.1.1 Number of cookies:

Null hypothesis: There is no difference between number of cookies assigned to experiment and control group.

We can use two ways to approach this question: 

1. **One-sample proportion**: compare exp/exp+control proportion with 0.5
2. **Paired t-test** to compare the difference between two groups and zero.

I used paired t-test below:

```jsx

import numpy as np
from scipy import stats

# Sample data (replace with actual values)
sample_data = group_b-group_a
n = len(sample_data)

# Step 1: Calculate the sample mean
sample_mean = np.mean(sample_data)

# Step 2: Calculate the standard error of the mean
sample_sd = np.std(sample_data, ddof=1)  # sample standard deviation
SE = sample_sd / np.sqrt(n)

# Step 3: Determine the critical value for a 95% CI
t_alpha_half = stats.t.ppf(0.975, df=n-1)

# Step 4: Construct the confidence interval
CI = (sample_mean - t_alpha_half * SE, sample_mean + t_alpha_half * SE)

print(f'Sample Mean: {sample_mean}')
print(f'Confidence Interval: {CI}')

Sample Mean: -23.864864864864863
Confidence Interval: (-58.19419839055342, 10.464468660823691)
```

### 4.1.2 number of clicks:

Using same paired t-test above:

```
import numpy as np
from scipy import stats

# Sample data (replace with actual values)
sample_data = control['Clicks']-exp['Clicks']
n = len(sample_data)

# Step 1: Calculate the sample mean
sample_mean = np.mean(sample_data)

# Step 2: Calculate the standard error of the mean
sample_sd = np.std(sample_data, ddof=1)  # sample standard deviation
SE = sample_sd / np.sqrt(n)

# Step 3: Determine the critical value for a 95% CI
t_alpha_half = stats.t.ppf(0.975, df=n-1)

# Step 4: Construct the confidence interval
CI = (sample_mean - t_alpha_half * SE, sample_mean + t_alpha_half * SE)

print(f'Sample Mean: {sample_mean}')
print(f'Confidence Interval: {CI}')

Sample Mean: 1.4324324324324325
Confidence Interval: (-4.924157226522307, 7.789022091387172)
```

### 4.1.3 click-through-probability (clicks/cookies):

```
import numpy as np
from scipy import stats

# Sample data (replace with actual values)
sample_data = control['Clicks']/control['Pageviews']-exp['Clicks']/exp['Pageviews']
n = len(sample_data)

# Step 1: Calculate the sample mean
sample_mean = np.mean(sample_data)

# Step 2: Calculate the standard error of the mean
sample_sd = np.std(sample_data, ddof=1)  # sample standard deviation
SE = sample_sd / np.sqrt(n)

# Step 3: Determine the critical value for a 95% CI
t_alpha_half = stats.t.ppf(0.975, df=n-1)

# Step 4: Construct the confidence interval
CI = (sample_mean - t_alpha_half * SE, sample_mean + t_alpha_half * SE)

print(f'Sample Mean: {sample_mean}')
print(f'Confidence Interval: {CI}')

Sample Mean: -6.124629702468428e-05
Confidence Interval: (-0.0006757077006900924, 0.0005532151066407238)
```

## 4.2 Evaluation Test

### 4.2.1 Paired t-test

1. Gross conversion

```jsx
import numpy as np
from scipy import stats

# Sample data (replace with actual values)
sample_data = exp['Enrollments']/exp['Clicks']-control['Enrollments']/control['Clicks']
n = len(sample_data)

# Step 1: Calculate the sample mean
sample_mean = np.mean(sample_data)

# Step 2: Calculate the standard error of the mean
sample_sd = np.std(sample_data, ddof=1)  # sample standard deviation
SE = sample_sd / np.sqrt(n)

# Step 3: Determine the critical value for a 95% CI
t_alpha_half = stats.t.ppf(0.975, df=n-1)

# Step 4: Construct the confidence interval
CI = (sample_mean - t_alpha_half * SE, sample_mean + t_alpha_half * SE)

print(f'Sample Mean: {sample_mean}')
print(f'Confidence Interval: {CI}')

```

Sample Mean: -0.020784582029265922
Confidence Interval: (-0.029775040939806888, -0.011794123118724955)

 SIGNIFICANT

1. Net conversion

```
import numpy as np
from scipy import stats

# Sample data (replace with actual values)
sample_data = exp['Payments']/exp['Clicks']-control['Payments']/control['Clicks']
n = len(sample_data)

# Step 1: Calculate the sample mean
sample_mean = np.mean(sample_data)

# Step 2: Calculate the standard error of the mean
sample_sd = np.std(sample_data, ddof=1)  # sample standard deviation
SE = sample_sd / np.sqrt(n)

# Step 3: Determine the critical value for a 95% CI
t_alpha_half = stats.t.ppf(0.975, df=n-1)

# Step 4: Construct the confidence interval
CI = (sample_mean - t_alpha_half * SE, sample_mean + t_alpha_half * SE)

print(f'Sample Mean: {sample_mean}')
print(f'Confidence Interval: {CI}')
```

Sample Mean: -0.004896856989809378
Confidence Interval: (-0.015343649839022363, 0.005549935859403606)

NOT SIGNIFICANT

### 4.2.2 Sign Test

Using Binomial test:

1. Gross conversion

```jsx
con1=control.copy()
exp1=exp.copy()

con1=con1.dropna(subset=['Enrollments', 'Clicks'])
exp1=exp1.dropna(subset=['Enrollments', 'Clicks'])
enroll_sign=exp1['Enrollments']/exp1['Clicks']-con1['Enrollments']/con1['Clicks']>0
print(sum(enroll_sign)/len(enroll_sign))

import numpy as np
from scipy import stats

# Given values
p_sample = 0.17
p_population = 0.5
n = 37

# Standard Error of the Proportion
SE_proportion = np.sqrt(p_population * (1 - p_population) / n)

# t-Statistic
t_statistic = (p_sample - p_population) / SE_proportion

# Degrees of Freedom
df = n - 1

# p-value
p_value = 2 * stats.t.sf(abs(t_statistic), df)

print(f'T-statistic: {t_statistic}, P-value: {p_value}')
print(SE_proportion)
print('confidence interval',p_population-SE_proportion, p_population+SE_proportion)
```

0.08219949365267865

T-statistic: -4.014623269996824, P-value: 0.0002888812852649517
confidence interval 0.41780050634732135 0.5821994936526786

**Significant**. Have enough evidence to reject null hypothesis.

1. Net conversion

```jsx

pay_sign=exp1['Payments']/exp1['Clicks']-con1['Payments']/con1['Clicks']>0
print(sum(pay_sign)/len(pay_sign))

import numpy as np
from scipy import stats

# Given values
p_sample = 0.43
p_population = 0.5
n = 37

# Standard Error of the Proportion
SE_proportion = np.sqrt(p_population * (1 - p_population) / n)

# t-Statistic
t_statistic = (p_sample - p_population) / SE_proportion

# Degrees of Freedom
df = n - 1

# p-value
p_value = 2 * stats.t.sf(abs(t_statistic), df)
print(SE_proportion)
print(f'T-statistic: {t_statistic}, P-value: {p_value}')
print('confidence interval',p_population-SE_proportion,p_population+SE_proportion)
```

0.08219949365267865
T-statistic: -0.8515867542417508, P-value: 0.4000729895961591
confidence interval 0.41780050634732135 0.5821994936526786

Not Significant. Don’t have enough evidence to reject null hypothesis.

# 5. Conclusion

In conclusion, I recommend launching this change in the production environment. It can help Udacity reach the original goal:

- Improving the experience of the rest users of “start free trials” since Udacity has less users to share the coaching services.

Beyond the one goal, there are three extra benefits:

- With the users’ experience improved, Udacity may attract more potential users.
- Or Udacity can keep the user experience at the same level and reduce the current coaching teams in order to decrease the cost.
- The change will not make a directly tremendous impact on the current revenue.
