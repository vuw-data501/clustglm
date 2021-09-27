load("../../data/whittaker.RData")
whit.df <- mat2df(whittaker$counts)
names(whit.df) <- c("counts","species","site")