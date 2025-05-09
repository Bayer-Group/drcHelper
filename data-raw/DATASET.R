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



pvi_example <- read.table(textConnection("Obs	rep	dose	yt  y0
1	1	2	0.02021	0
2	2	2	0.02491	0
3	3	2	0.00760	0
4	5	2	0.04466	0
5	6	2	0.00037	0
6	8	2	0.05386	0
7	9	2	0.07205	0
8	10	2	0.01125	0
9	1	4	0.02011	0
10	7	4	0.09058	0
11	8	4	0.06255	0
12	10	8	0.09431	0
13	4	2	0.14060	A
14	7	2	0.22223	A
15	2	4	0.20943	A
16	3	4	0.20959	A
17	4	4	0.17305	A
18	9	4	0.11405	A
19	10	4	0.17668	A
20	1	8	0.12756	A
21	6	8	0.11478	A
22	6	32	0.20602	A
23	5	4	0.26650	B
24	6	4	0.27344	B
25	3	8	0.27021	B
26	5	8	0.30662	B
27	8	8	0.29319	B
28	9	8	0.37300	B
29	6	16	0.36224	B
30	9	16	0.31316	B
31	2	8	0.55845	C
32	4	8	0.44811	C
33	3	16	0.42677	C
34	3	32	0.52315	C
35	7	8	0.67080	D
36	2	16	0.71776	D
37	4	16	0.73038	D
38	5	16	0.64232	D
39	7	16	0.68720	D
40	8	16	0.61088	D
41	5	32	0.72342	D
42	8	32	0.63594	D
43	1	16	0.77171	E
44	10	16	0.74087	E
45	2	32	0.79477	E
46	4	32	0.88546	E
47	7	32	0.78002	E
48	9	32	0.81456	E
49	10	32	0.89465	E
50	1	32	0.96129	F
51	1	64	0.96127	F
52	2	64	0.91687	F
53	3	64	0.97204	F
54	4	64	0.99268	F
55	5	64	0.98935	F
56	6	64	0.96263	F
57	7	64	0.95435	F
58	8	64	0.92081	F
59	9	64	0.91776	F
60	10	64	0.99104	F"),header = TRUE)

pvi_example <- pvi_example %>% mutate(yy = as.numeric(plyr::mapvalues(y0,from = c("0","A","B","C","D","E","F"),to = c(0,10,30,50,70,90,100)))/100) %>% mutate(yy2 = as.numeric(plyr::mapvalues(y0,from = c("0","A","B","C","D","E","F"),to = c(0.05,0.18,0.34,0.50,0.66,0.82,0.95))))

usethis::use_data(pvi_example,overwrite = TRUE)

