% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drc_Helper.R
\name{addECxCI}
\alias{addECxCI}
\title{adding ECx estimation and interval to the model output plot}
\usage{
addECxCI(
  p = NULL,
  object,
  EDres = NULL,
  trend = "Decrease",
  endpoint = "ErC",
  respLev = c(10, 20, 50),
  textAjust.x = 0.1,
  textAjust.y = 0.05,
  useObsCtr = FALSE,
  d0 = NULL,
  textsize = 2,
  lineheight = 1,
  xmin = 0.05,
  ...
)
}
\arguments{
\item{p}{model plot as ggplot2 output}

\item{object}{fitted drc object}

\item{EDres}{EDresults corresponding to respLev}

\item{trend}{"Decrease" or "Increase"}

\item{endpoint}{ErC or EbC or EyC or EC or LD}

\item{respLev}{reponse levels}

\item{textAjust.x}{where to label ECx}

\item{textAjust.y}{where to label ECx}

\item{useObsCtr}{whether to use observed control mean}

\item{d0}{to be used control mean.}

\item{textsize}{label text size}

\item{lineheight}{errorbar height}

\item{xmin}{when confidence intervals very wide including even negative values.}

\item{...}{additional inputs passed into ED functions.}
}
\value{
ggplot object with added ECx CIs.
}
\description{
adding ECx estimation and interval to the model output plot
}
\examples{
\dontrun{
data("dat_medium")
dat_medium <- dat_medium \%>\% mutate(Treatment=factor(Dose,levels=unique(Dose)))
dat_medium$Response[dat_medium$Response < 0] <- 0
mod <- drm(Response~Dose,data=dat_medium,fct=LN.4())
p1 <- plot.modList(list(mod))
addECxCI(p1,object=mod,EDres=NULL,trend="Decrease",endpoint="EC", respLev=c(10,20,50),
textAjust.x=0.01,textAjust.y=0.3,useObsCtr=FALSE,d0=NULL,textsize = 4,lineheight = 0.5,xmin=0.012)+
ylab("Response Variable [unit]") + xlab("Concentration [µg a.s./L]")
}
}
