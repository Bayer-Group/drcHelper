# In R/zzz.R ## a hack to avoid S3 method warning. These are not actually S3 methods.
.onLoad <- function(libname, pkgname) {
  # Register non-S3 methods to avoid CRAN check warnings
  ns <- asNamespace(pkgname)

  # For ED.plus and ED.ZG
  if (exists("ED.plus", ns)) {
    registerS3method("ED", "plus", utils::getFromNamespace("ED.plus", pkgname), envir = ns)
  }

  if (exists("ED.ZG", ns)) {
    registerS3method("ED", "ZG", utils::getFromNamespace("ED.ZG", pkgname), envir = ns)
  }
  if (exists("plot.modList", ns)) {
    registerS3method("plot", "modList", utils::getFromNamespace("plot.modList", pkgname), envir = ns)
  }
}
