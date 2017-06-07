# ices_introstockassessment
Introduction to stock assessment​  5 - 9 June 2017, ICES, Copenhagen, Denmark


Example code to upload to and download from the bluebridge VRE workspaces

```r
# ------------------------------------
#
#  Quick script to download the R code for the demonstration
#
# ------------------------------------

library(httr)
library(XML)

# load in vre functions
while("tools:vre_funs" %in% search()) detach("tools:vre_funs")
sys.source("vre_tools.R", envir = attach(NULL, name = "tools:vre_funs"))

# get root Workspace
ws <- listHomeWS()

# rdemo folder
rdemo <- paste0(ws, "/ICES_IntroStockAssessment/R-demo")

# download workspace
url <- downloadURI(rdemo)
#x <- httr::GET(url)


# upload files (here we are uploading the albacore data
files <-  "R-demo/albacore.dat"
for (file in files) {
  uploadWSManager(rdemo, file, overwrite = TRUE, archive = FALSE) #UPLOAD THE FILE TO THE WS
}
```
