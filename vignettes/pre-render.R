# To update the vignette:
knitr::knit("vignettes/select.Rmd.orig", output = "vignettes/select.Rmd")

# To create a script that uploads the files:
knitr::purl("vignettes/select.Rmd.orig", output = "vignettes/select.R")

