
# Packages ----------------------------------------------------------------
library(shiny)
library(drc)
library(ggplot2)
library(shinyjs)
library(esmisc)
library(plotly)
library(markdown)

# drc models --------------------------------------------------------------
mods <- getMeanFunctions(display = FALSE)
select_mod <- sapply(mods, '[', 1)
names(select_mod) <- paste0(sapply(mods, '[', 1), 
                            ': ',
                            sapply(mods, '[', 2))



# parse textInput ---------------------------------------------------------
numextractall <- function(string) { 
  # http://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string
  as.numeric(unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)), 
                    use.names = FALSE))
} 


# Calc x-breaks -----------------------------------------------------------

x <- 11:1001
xmin <- min(x)
xmax <- max(x)
get_breaks <- function(x) {
  10^(seq(floor(log10(xmin)), floor(log10(xmax))))
}

