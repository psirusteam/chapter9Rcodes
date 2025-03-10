# Introduction {-}

A key concern for every agency that produces statistical information is ensuring the *correct* use of the data it provides. This concern is enshrined in the United Nations *Fundamental Principles of Official Statistics*, particularly in the following principles:

- **Principle 3:** To facilitate a correct interpretation of the data, statistical agencies must present information according to scientific standards, including details on the sources, methods, and procedures used.
- **Principle 4:** Statistical agencies are entitled to comment on erroneous interpretation and misuse of statistics.

The advent of the computer revolution, coupled with greater access to computational tools, has led to increased use of statistical data, including household survey data. Sometimes this data is used for mostly *descriptive purposes*, such as estimating population means or obtaining estimates of population frequency distributions. Other times, however, its use is made for *analytical purposes*, involving the testing of hypothesis or the construction of models, when the objective is to draw conclusions that are also applicable to populations other than the one from which the sample was extracted. When using standard statistical software for such analyses, results can be biased or misleading if the complex sampling design is not properly accounted for.

Household surveys also play a critical role in tracking progress toward global objectives, such as the **Sustainable Development Goals (SDGs)**. For this purpose, descriptive analyses often include a range of specialized indicators designed to monitor outcomes like access to education, health services, and economic opportunities. These indicators are derived from the survey data and are essential for policymakers and organizations aiming to achieve sustainable development targets.

This chapter aims to empower users to analyze household survey data accurately and effectively. It does this by presenting relevant models, methods, and software that enables the data analyst to understand key steps in the data analysis process and to incorporate complex designs into their analyses. It relies on the fundamental concepts of the design-based paradigm for survey design and analysis.

What makes household survey data special or challenging for those who intend to analyse them is because they are collected through complex sampling methods that often involve:

- **Stratification**: Dividing the population into comprehensive distinct subgroups before sampling;
- **Clustering**: Grouping units and sampling groups rather than units to simplify data collection;
- **Unequal probabilities of selection**: Giving units different probabilities of being selected;
- **Weighting adjustments**: Correcting for non-response and/or improving precision.

Standard data analysis methods and software ignore these features, leading to biased estimates of both the target parameters and their associated variances. Here we analyze the impact of simplifications made when using standard data analysis methods and software, and present the necessary adjustments to these methods in order to appropriately incorporate the aspects highlighted above into the analysis. 

Different readers of the chapter may find some of its parts more useful than others. Here we describe contents of each of the sections, so that readers may direct their attention to the topics of relevance to them. Section 9.1 provides guidance on the preparation of a plan for the analysis of the household survey data. Such plans are important both to guide survey development and subsequently any secondary survey data analysis. 

Section 9.2 provides a short discussion on the fundamental principles of the design-based inference, emphasizing that conclusions taken from probability sample surveys should be based on a pair: the point estimate and it associated margin of error (or any related measure). Section 9.3 begins the journey with the key tools for descriptive analysis: means, ratios, proportions and other typical descriptive parameters. 

Section 9.4 is devoted to more complex parameters that allow comparisons of the phenomenon of interest between subgroups for continuous and discrete variables. It also presents standard tests to compare means and measure the degree of correlation and association between variables. 

Section 9.5 focuses on modelling survey outcomes. It starts with a discussion on the role of weighting when estimating regression coefficients, followed by presentation of some proper approaches to estimate complex parameters in linear and logistic regression models. Finally, section 9.6 presents a summary of ideas and tools for survey data visualization, showing the best practices for creating graphics in a context where sampling imbalance and uncertainty are important.

Throughout the chapter, practical examples are provided to illustrate how National Statistics Offices (NSOs) conduct various types of household survey data analysis. These examples provide a useful guide for applying the concepts and methods discussed in real-world contexts. By the end of the chapter, readers will be equipped with the knowledge and tools needed to analyze household survey data effectively while accounting for the complexities of survey designs adopted in such surveys.
