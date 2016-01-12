# =================================
# mini_functions.R
# =================================

require(quanteda)
require(dplyr)
require(stringi)
require(stringr)
DEBUG <- FALSE


getID <- function(in_word) {
    if (is.na(in_word)) return(0)
    id <- ifelse(is.na(voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"]),
                "0",
                voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"])
    id <- ifelse(is.null(voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"]),
                 "0",
                 voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"])
    return(id)
}

getWord <- function(in_id) {
    if (ia.na(in_id)) return("unk")
	word <- ifelse(is.na(voc_mini_df[which(voc_mini_df$id == in_id), "voc_term"]),
                "unk",
                voc_mini_df[which(voc_mini_df$id == in_id), "voc_term"])
    word <- ifelse(is.na(voc_mini_df[which(voc_mini_df$id == in_id), "voc_term"]),
                "unk",
                voc_mini_df[which(voc_mini_df$id == in_id), "voc_term"])
    return(word)
}

get_NG_id <- function(ng) {
    n_word <- c(" ")
    ng_list <- str_split(ng, " ")
    if (DEBUG == TRUE) print(ng_list)

    for (j in 1:lengths(ng_list)) {
        nw <- sapply(ng_list, "[[" , j)
        if (DEBUG == TRUE) cat(i, " word:", nw, "\n")
        n_id <- getID(nw)
        if (DEBUG == TRUE) cat("....n_id", n_id, "\n")
        n_word <- paste(n_word, n_id)
        if (DEBUG == TRUE) cat("....n_word:", n_word, "\n")
    }
    return(n_word)
}

get_NG_word <- function(ng_id) {
    n_word <- c(" ")
    ng_list <- str_split(ng_id, " ")
    if (DEBUG == TRUE) print(ng_list)

    for (j in 1:lengths(ng_list)) {
        n_id <- sapply(ng_list, "[[" , j)
        if (DEBUG == TRUE) cat(i, " n_id:", n_id, "\n")
        n_wd <- getWord(n_id)
        if (DEBUG == TRUE) cat("....n_wd", n_wd, "\n")
        n_word <- paste(n_word, n_wd)
        if (DEBUG == TRUE) cat("....n_word:", n_word, "\n")
    }
    return(n_word)
}
