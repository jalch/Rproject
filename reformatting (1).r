library(RUnit)
errMsg <- function(err) print(err)
load('reformatting-tests.rda')



makeBinary <- function(response.row, n.responses) {
	list.binary <- mapply(function(resp.row,n) {
		if(resp.row == 0) rep(0,n)
		else c(rep(0,resp.row - 1), 1, rep(0, n - resp.row))
		}, response.row, n.responses) 
	binary <- unlist(list.binary)
	return(binary)

}


is dataframe as the file
# "binary-ling-data.data".


ling.data.clean <- read.table("ling-data-clean.data", header=T)
num.responses <- unname(apply(ling.data.clean,2,max))
binary.ling.data <- t(apply(ling.data.clean,1, function(x) makeBinary(x, num.responses)))
#dec <- unlist(sapply(num.responses/10, function(i) seq(.1, i, by = .1)))
names.dec <- sapply(1:length(num.responses), function(i) paste(num.responses[i],".", dec[i], sep =""))
#colnames(binary.ling.data) <- names.dec
binary.form <- cbind(ling.data.clean[,c(1:4, 72, 73)], binary.ling.data)
write.table(binary.form, "binary-ling-data.data", row.names = FALSE)











