---
title: "CHAPTER 9 - EXAMPLES IN R"
author: ""
date: "2025-03-10"
documentclass: book
bibliography: chapterbib.bib
biblio-style: apalike
link-citations: yes
colorlinks: yes
lot: yes
lof: yes
fontsize: 12pt
geometry: margin=3cm
github-repo: psirusteam/chapter9Rcodes
description: "CHAPTER 9 - EXAMPLES IN R"
knit: "bookdown::render_book"
linkcolor: blue
output: 
  bookdown::pdf_book:
    toc: true
    toc_depth: 1
    keep_tex: true
    latex_engine: xelatex
---




# Abstract {-}

Analyzing complex household survey data requires knowing and properly applying the foundations of *design-based inference*. The researcher will be faced to a small dataset containing information that will allow her to make conclusions over the whole population. 

The purpose of any analysis on this kind is not to make conclusions about the sample itself – which in most of the cases is a small subset of the population - but to the the whole population and possibly domains or population subgroups of interest. Having that into account, the first step in any analysis plan should be devoted to understanding and specifying the sampling design used to draw the sample and the findings on the field related to nonresponse and lack of coverage. 

This chapter covers three main topics of analysis: descriptive statistics; comparisons and associations; and modeling of survey data. First, we introduce simple descriptive statistics, such as totals, means, frequencies and proportions, quantiles and some graphics; next, we delve deeper on relationships between the survey variables. All these analyses rely on the representativity principle of the design-based inference. This way, the reader will find a strong focus, not only on point estimates, but also on uncertainty measures. The chapter also presents a short discussion on the different approaches that can be used to estimate variances; the best way to visualize the estimates; and some NSO practical experiences.
