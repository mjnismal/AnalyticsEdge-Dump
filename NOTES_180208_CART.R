---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

data = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/80dfa0ae5eb3fa2013a34507cd58fabb/asset-v1:MITx+15.071x+2T2017+type@asset+block/stevens.csv")
head(data)
```

```{r}
library(caTools)
set.seed(3000)
spl

```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
