# A&E Modelling

## Summary & Scope

This is a simple approach to modelling A&E breaches. Since COVID-19, the proportion of people waiting longer than 4hrs across all A&E settings have increased dramatically. Across England, A&E departments were given a target of ensuring that the proportion of 'breaches' are kept to less than 5% of total A&E attendances, up from a target of 98% pre-2010. The last time this target was met, however, was in 2011, with the number of breaches substantially worsening particularly in the aftermath of COVID-19. 

These trends have been partially driven by an increase in occupied beds, with [previous research](https://www.kingsfund.org.uk/insight-and-analysis/long-reads/whats-going-on-with-ae-waiting-times)  demonstrating that a larger number of occupied beds limits the ability of A&E departments to discharge patients appropriately, resulting in longer waits and, subsequently, more breaches.  However, this relationship is not entirely clear. [Previous DHSC research](https://webarchive.nationalarchives.gov.uk/ukgwa/+/www.dh.gov.uk/en/Publicationsandstatistics/Publications/AnnualReports/Browsable/DH_4989760) from the early 2000's suggested that 92% occupancy is the limit until departments struggle with A&E admissions. Conversely, whilst many trusts met their targets whilst operating at 85% occupancy pre-COVID, post-COVID many are struggling to achieve the same breach rates as in the years prior. 

This suggests there is fundemental change since 2019/20 in terms of the ability of A&E trusts to deliver the same level of care either in terms of supply (due, for eg, to capacity constraints) or demand (due to a more complex casemix). It also reinvigorates debate about whether investment into opening more beds is still optimal in the aftermath of the pandemic, and what are the potential savings in terms of breaches in returning to pre-COVID bed occupancy rates.

This work thus seeks to build a basic model underlying the patterns behind A&E breaches and to estimate the marginal effect of an increase in occupied bed rates across different organisations and controlling for both the pre and post-Covid effect.

## Approach and Methodology

This project utilises publically available data which is both wrangled and web-scraped using the [Open Data project](https://github.com/zeyadissa/open_health_data). The underlying approach is based off of a Zero/One Inflated Beta Regression, which utilises a simple bayesian approach using the brilliant brms package. 

The conceptual backing behind using a Beta Regression is two-fold:

1. The usage of proportions complicates the use of any linear model due to the fact it is constrained (wherein the model is not) - this has implications about the variance of the data and is therefore unwise. A beta regression enables a 0-1 constrained model.
2. The impact of changing bed occupancy ratios on A&E breaches is not only expected to change under different levels of occupancy, but the rate of which it changes as well. Therefore, the prior assumption is that both the mean estimation and the shape will vary. A beta regression is effective as the precision variable enables it to take a variety of shapes, and thus this approach enables us to model both a change in mean **and** a change in precision.

The bayesian approach was preferred over, say, a fractional logistic model due to the limited number of data points (~7,000) and the complex heterogeneities likely present in the data. However, the results were nevertheless validated using a fractional logistic regression and yielded similar results.

## General Findings

The findings are stark in that they reveal three particular trends:

1. The impact of opening beds is positively associated with reducing A&E breaches, in line with previous research
2. The impact of changing occupied-bed ratios has changed substantially since COVID-19, with freeing up beds associated with nearly 2-3x less breaches than pre-2019/20
3. The impact of opening beds is is insignificant at very low levels <60% occupancy, but grows exponentially, reaching a nadir at 100%

Estimates of the average proportion of breaches for each occupied ratio by period (Pre, post, and during COVID) can be calculated for the mean trust.

![Alt Text](https://github.com/zeyadissa/ae_modelling/blob/main/res/estimates.png)

This is furthered by the analysis of marginal effects, which reveals an increasing trend with wider dispersion across higher occupancy rates.

![Alt Text](https://github.com/zeyadissa/ae_modelling/blob/main/res/marginal_effects.png)

## Limitations & Further Research

Though the findings are significant, they are nevertheless limited. Due to the lack of site-specific information, particularly in relation to staffing, and the usage of public data, this analysis makes some leaps of logic. Further research can be targetted towards building a capacity approach using an active production function: by identifying the proportion of patients likely to need admittance from patient-record level data, modelling that across time, and identifying both the time taken to admit someone and the external limitations arising due to high bed occupancy.
