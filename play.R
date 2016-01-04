library(stringr)
library(dplyr)
library(sqldf)

vc1 <- readRDS("Data/voc_colo.RDS")
vc2 <- vc1

r <- sqldf('select v1.word1, v1.word2, v1.word3, v2.word1, v2.word2, v2.word3
           from vc1 as v1, vc2 as v2 where v1.word3 = v2.word1
           and v1.word1 = "department"')

# status <- function(txt) {
#     t <- as.character(str_split_fixed(txt, " ", n = 5))
#     for (i in 1:5) {
#         if (!t[i] == "" && !t[i] == "[0-9]*") {
#             m <- c(t[i])
#             err <- TRUE
#         } else {
#             err <- FALSE
#         }
#     }
#     return (c(t[1], t[2], t[3], err))
# }
#
# o <- status("1 boy played the")
# print(o)