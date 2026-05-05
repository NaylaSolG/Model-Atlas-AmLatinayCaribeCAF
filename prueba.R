---
  title: "Análisis territorial - Atlas CAF Argentina"
author: "Nayla Sol Garcilazo"
date: "2025-12-28"
output: html_document
---
  
  ```{r setup}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)

library(tidyverse)
library(corrplot)
library(viridis)
library(tidymodels)
library(broom)