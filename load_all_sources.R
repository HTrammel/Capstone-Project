# ----------------------------------------
# Load everything
# ----------------------------------------

require(quanteda)
require(stringr)
require(dplyr)
require(data.table)

#----------------------------------------------------------------------
# The following were provided by Ng Kuang Chern Nathaniel in the forums
#----------------------------------------------------------------------
library(parallel)
library(stringi)
my.cleaning.function <- function(text) {
    # function to deal with accented chars/unicode/numbers/punctuation/...
    text <- stringi::stri_trans_general(text, "latin-ascii")
    #text <- chartr(..., text)
    text <- gsub("&", "and", text)
    tolower(text)
}

parallel.clean <- function(text) {
  cl <- makeForkCluster(3)
  clusterExport(cl, "my.cleaning.function")
  clusterEvalQ(cl, library(stringi))
  text.clean <- parSapply(cl, text, my.cleaning.function, USE.NAMES=FALSE)
  stopCluster(cl)
  text.clean
}
#----------------------------------------------------------------------

src <- "Data/final/en_US/en_sources.txt"

con <- file(src, open="rb")
tmp <- scan(con,
            what = "complex",
            encoding = "UTF-8",
            blank.lines.skip = TRUE,
            skipNul = TRUE)
tmp <- parallel.clean(tmp)
saveRDS(tmp, "Data/en_total_src.Rds")
close(con)
