## code to prepare `DATASET` dataset goes here

## metal data

## Note the algae SAS dataset has been moved to the internal data package drcBAG.
## The script is kept as an example to read in SAS datasets.
# install.packages("foreign")
#
# # Load the foreign package
# library(foreign)
# library(haven)
# # Read the SAS dataset
# algae1 <- read.ssd("~/Projects/drcHelper/data-raw/algae.sas7bdat")
# algae1 <- read_sas("~/Projects/drcHelper/data-raw/algae.sas7bdat")
#
# # View the first few rows of the dataset
# head(algae1)

metaldata<-read.csv(file="~/Projects/drcHelper/data-raw/metaldata.csv")
usethis::use_data(metaldata, overwrite = TRUE)


## oecd201 example dataset
oecd201 <- read.csv("~/Projects/drcHelper/data-raw/OECD_201.csv")
oecd201$Treatment <- factor(oecd201$Treatment,levels=unique(oecd201$Treatment))
usethis::use_data(oecd201, overwrite = TRUE)



## BCS 1 for RSCABS demonstration
library(rio)
library(knitr)
library(tidyverse)
dat_bcs1 <- import("~/Projects/drcHelper/data-raw/bcs1 fstra female histopath.xlsx") %>%
  fill(tmt) %>%
  dplyr::select(tmt, id, oocyte_atr) %>%
  mutate(tank = substr(id, 1, 2),
         oocyte_atr = paste0("S", oocyte_atr)) %>%
  count(tmt, tank, oocyte_atr) %>%
  pivot_wider(names_from = oocyte_atr, values_from = n) %>%
  mutate(across(starts_with("S"), ~ ifelse(is.na(.), 0, .)),
         S0_1 = S0 + S1,
         S0_2 = S0 + S1 + S2,
         total = S0 + S1 + S2 + S3)
usethis::use_data(dat_bcs1, overwrite = TRUE)


## Test Cases for V-COP / Validated run environment

test_cases_res <- readxl::read_excel("data-raw/R-V-Cop_test_cases_level_2_redact.xlsx", sheet="test cases")
test_cases_data <- readxl::read_excel("data-raw/R-V-Cop_test_cases_level_2_redact.xlsx",sheet="EFX statistics")
usethis::use_data(test_cases_data, overwrite = TRUE)
usethis::use_data(test_cases_res, overwrite = TRUE)



DixonQ <- read.table(textConnection("n	Q_critical
3	0.941
4	0.765
5	0.642
6	0.56
7	0.507
8	0.554
9	0.512
10	0.477
11	0.576
12	0.546
13	0.521
14	0.546
15	0.525
16	0.507
"),header =TRUE)

usethis::use_data(DixonQ)

