###################################################################
# Profanity Filtering
#
# profanity filtering is based on:
# FrontGate Media,Your Gateway to the Chrisitan Audience
# www.FrontGateMedia.com,Terms to Block for social interaction
###################################################################

block_list <- readLines("Data/Terms-to-Block.txt")
block_dictionary <- dictionary(list(blocked = block_list))