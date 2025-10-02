#' Williams Test Lookup Table
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"williamsTestLookUpTable"

#' test_cases_data for validation purpose
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"test_cases_data"

#' test_cases_res for validation purpose
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"test_cases_res"

#' exampleHistData from StatCharrms
#'
#' @docType data
#' @keywords datasets
"exampleHistData"


#' example data with shallow dose-response
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"dat_shallow"


#' example data with steep dose-response
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"dat_steep"

#' example data with medium dose-response
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"dat_medium"

#' example data with no ED 50 response
#'
#' @docType data
#' @keywords datasets
#' @rdname drcHelper_datasets
"dat_noED50"


#' An example dataset from study type OECD 201
#'
#' @name oecd201
#' @author BCS
#' @docType data
#' @keywords datasets
#' @format A data frame with 112 rows and 14 columns
#' @details
#' The data is originally provided by Ecotox colleague Andreas and was preprocessed so that GrowthRate is included.
#'
#' @references OECD Test No. 201: Freshwater Alga and Cyanobacteria, Growth Inhibition Test \url{https://www.oecd.org/en/publications/test-no-201-alga-growth-inhibition-test_9789264069923-en.html}
NULL

#' NTP_example data
#'
#' @docType data
#' @keywords datasets
#' @format NTP example data with corrected and uncorrected emergence, survival, reduction.
"NTP_example"

#' Expected processed data from the NTP_example
#'
#' @docType data
#' @keywords datasets
#' @format NTP example rate data
#' @keywords deprecated
"NTP_example_rate"

#' Expected outcome of NTP
#'
#' @docType data
#' @keywords datasets
"NTP_example_rate_expected"

#' Expected outcome of pre-processing NTA_Ar_ext mortality
#'
#' @docType data
#' @keywords datasets
"NTA_Ar_ext_mortality_expected"



#' Dixon's outlier test critical Q table
#'
#' @author Zhenglei Gao
#' @references DIXON, W. J. (1950) Analysis of extreme values. Ann. Math. Stat. 21, 488-506.
#' DEAN, R. B., DIXON, W. J. (1951) Simplified statistics for small numbers of observation. Anal. Chem. 23, 636-638.
"DixonQ"


#' Hamilton dose-response datasets
#'
#' Example dose-response data given in Hamilton (1977).
#' Note that, as per Hamilton (1978), the confidence intervals
#' given in Hamilton (1977) for these data sets are incorrect.
#'
#' @author B R S Recht
#' @docType data
#' @keywords datasets
#' @format A list containing ten data frames: dr1a, dr1b, dr1c,
#' dr1d, dr1e, dr4a, dr4b, dr4c, dr4d, dr4e
#' @source Hamilton, 1977.
#' @references \url{https://github.com/brsr/tsk}
"hamilton"


#' Fake data from collembola juveniles
#'
#' @docType data
#' @keywords datasets
#' @format collembola_juveniles in wide format
"collembola_juveniles"


#' Fake data as an example of PVI data
#'
#' @docType data
#' @keywords datasets
#' @format pvi data
"pvi_example"


#' Fake data as an example of PVI data
#'
#' @docType data
#' @keywords datasets
#' @format pvi data
"dat_bcs1"


#' Data from Acute Studies
#'
#' Data are from a study of the response of the daphnids. There are 6 treatment
#' groups and 6 replicates in each treatment group, 5 individuals in each replicates.
#'
#' @docType data
#' @keywords datasets
"quantal_dat_nested"


#' Data from heavy metal mixture experiments (drcData)
#'
#' Data are from a study of the response of the cyanobacterial
#' self-luminescent metallothionein-based whole-cell biosensor
#' Synechoccocus elongatus PCC 7942 pBG2120 to binary mixtures of
#' 6 heavy metals (Zn, Cu, Cd, Ag, Co and Hg).
#'
#' @docType data
#' @keywords datasets
"metaldata"
