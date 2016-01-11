#=================================================
# map_vocab_ngrams_v1r1.R
#=================================================

require(quanteda)
require(dplyr)
require(stringr)
DEBUG <- FALSE

if (!exists("voc_mini_df")) {
	voc_mini_df <- readRDS("Data/voc_mini_df.Rds")
}
#if (!exists("ng_mini_df")) {
	ng_mini_df <- readRDS("Data/ng_mini_df.Rds")
#}

getID <- function(in_word) {
	id <- voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"]
}

getWord <- function(in_id) {
	word <- voc_mini_df[which(voc_mini_df$id == in_id), "voc_term"]
}

for (i in 1:nrow(ng_mini_df)) {
    t_word <- c(" ")
    tw_list <- str_split(ng_mini_df[i,1], " ")
    if (DEBUG == TRUE) print(tw_list)

    for (j in 1:lengths(tw_list)) {
        tw <- sapply(tw_list, "[[" , j)
        if (DEBUG == TRUE) cat(i, " word:", tw, "\n")
        tmp_id <- getID(tw)
        if (DEBUG == TRUE) cat("....tmp_id", tmp_id, "\n")
        t_word <- paste(t_word, tmp_id)
        if (DEBUG == TRUE) cat("....t_word:", t_word, "\n")
    }
    ng_mini_df[i, 4] <- t_word
    if (DEBUG == TRUE) print(paste("....row ng_id:", ng_mini_df[i,4]))
}
