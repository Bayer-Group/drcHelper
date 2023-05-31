## code to prepare `DATASET` dataset goes here

oecd201 <- read.csv("~/Projects/drcHelper/data-raw/OECD_201.csv")
oecd201$Treatment <- factor(oecd201$Treatment,levels=unique(oecd201$Treatment))
usethis::use_data(oecd201, overwrite = TRUE)



## PTZ
library(rio)
library(knitr)
library(tidyverse)
dat_ptz <- import("~/Projects/drcHelper/data-raw/ptz fstra female histopath.xlsx") %>%
  fill(tmt) %>%
  select(tmt, id, oocyte_atr) %>%
  mutate(tank = substr(id, 1, 2),
         oocyte_atr = paste0("S", oocyte_atr)) %>%
  count(tmt, tank, oocyte_atr) %>%
  pivot_wider(names_from = oocyte_atr, values_from = n) %>%
  mutate(across(starts_with("S"), ~ ifelse(is.na(.), 0, .)),
         S0_1 = S0 + S1,
         S0_2 = S0 + S1 + S2,
         total = S0 + S1 + S2 + S3)
usethis::use_data(dat_ptz, overwrite = TRUE)
