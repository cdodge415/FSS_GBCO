setwd("/Volumes/Blaszczak Lab/FSS/Literature")
# bring in subset of Appendix 1 Table 1-6 from the Streamgage Matching USGS doc. made in excel
info <- read.csv("Table1-6_subset.csv")
class(info$State)
levels(info$State)
# subset for potential GBCO sites
info <- subset(info, info$State %in% states)
states <- c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY")
info$State <- factor(info$State)
levels(info$State)
# write subset to new csv
setwd("/Volumes/Blaszczak Lab/FSS/Analysis/Streamgage_matching")
write.csv(info, "Streamgage_Matches_subset.csv")

# once sites are FINALIZED we can see which of those sites are already matched here and can either 
# have their matches used, or need new matches