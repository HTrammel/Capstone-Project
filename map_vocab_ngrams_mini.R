#=================================================
# map_vocab_ngrams_v1r1.R
#=================================================
require(quanteda)
require(dplyr)
require(stringi)
require(stringr)
DEBUG <- FALSE

source("mini_functions.R")

if (!exists("voc_mini_df")) {
    voc_mini_df <- readRDS("Data/voc_mini_df.Rds")
}
if (!exists("ng_mini_df")) {
    ng_mini_df <- readRDS("Data/ng_mini_df.Rds")
}


if (DEBUG == TRUE) print ("Mapping ngrams")
z <- nrow(ng_mini_df)
for (i in 1:z) {
    cat(i, " of ", z, "\n")
    t_word <- c(" ")
    ng_cnt <- str_count(ng_mini_df[i,1], " ")
    if (DEBUG == TRUE) print(ng_cnt)

    if (ng_cnt > 0) {
        t_word <- get_NG_id(ng_mini_df[i,1])
        if (DEBUG == TRUE) cat("....t_word:", t_word, "\n")
    } else {
        tmp_id <- getID(ng_mini_df[i,1])
        if (DEBUG == TRUE) cat("....tmp_id", tmp_id, "\n")
        t_word <- tmp_id
        if (DEBUG == TRUE) cat("....t_word:", t_word, "\n")
    }
    ng_mini_df[i, 4] <- t_word
    if (DEBUG == TRUE) cat(paste("....row ng_id:", ng_mini_df[i,4], "\n","\n", sep=" "))
}
