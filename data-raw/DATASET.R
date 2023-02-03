## code to prepare `DATASET` dataset goes here

oecd201 <- read.csv("~/Projects/drcHelper/data-raw/OECD_201.csv")
oecd201$Treatment <- factor(oecd201$Treatment,levels=unique(oecd201$Treatment))
usethis::use_data(oecd201, overwrite = TRUE)
