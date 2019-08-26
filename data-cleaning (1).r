

#text to dataframe
lingData <- read.table("lingData.txt", header = TRUE, sep = "")

#subset of data with only question variables
q.subset <- lingData[-c(1:4,72,73)]


rm.omit.all <- q.subset[rowSums(q.subset) != 0,]



n.no.response <- nrow(q.subset) - nrow(rm.omit.all)



omit.per.row <- rowSums(rm.omit.all == 0)
hist(omit.per.row, main = 'Number Omitted Questions', xlab = 'Questions Omitted')





non.response.cutoff <- quantile (omit.per.row, .99)
ile = "ling-data-clean.data", row.names = FALSE)
