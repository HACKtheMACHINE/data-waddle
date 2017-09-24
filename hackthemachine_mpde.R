#### HACKtheMACHINE R Script #############################################
#
# Data for HACKtheMACHINE at MIT CSAIL Weekend Sep 22, 2017
#
# Author: Avory Bryant

library(RColorBrewer)
library("Rtsne")


# Assumes file is complete
M <- read.csv(file="classC_ship2_MPDE.csv")

M$DateTime <- as.numeric(as.POSIXlt(M$DateTime,format = "%m/%d/%Y %H:%M:%S %p"))

M$DateTime <- M$DateTime - min(M$DateTime)

timeindex <- which(names(M) == "DateTime")

fx1a <- c(timeindex,grep("X1A",names(M)))
#fx1b <- c(timeindex,grep("X1B",names(M)))
#fx2a <- c(timeindex,grep("X2A",names(M)))
#fx2b <- c(timeindex,grep("X2B",names(M)))

M1A <- M[M$Indicator == "MPDE1A",fx1a]
#M2A <- M[M$Indicator == "MPDE2A",fx2a]
#M1B <- M[M$Indicator == "MPDE1B",fx1b]
#M2B <- M[M$Indicator == "MPDE2B",fx2b]

#numna <- vector(length=nrow(M2A))
#for(i in 1:nrow(M2A)){
#	numna[i] <- sum(is.na(M2A[i,]))
#}
# print(unique(numna)) 10,68
#attr <- unique(which(!is.na(M2A[which(numna==68)[1],])))
#M2A_68 <- M2A[numna==68,c(1,attr)]
#attr <- unique(which(!is.na(M2A[which(numna==10)[1],])))
#M2A_10 <- M2A[numna==10,c(1,attr)]

# print(sum(is.na(M2A_68))) 0
# print(sum(is.na(M2A_10))) 0

numna <- vector(length=nrow(M1A))
for(i in 1:nrow(M1A)){
        numna[i] <- sum(is.na(M1A[i,]))
}
# print(unique(numna)) 10,68,45 (1)
attr <- unique(which(!is.na(M1A[which(numna==68)[1],])))
M1A_68 <- M1A[numna==68,c(1,attr)]
attr <- unique(which(!is.na(M1A[which(numna==10)[1],])))
M1A_10 <- M1A[numna==10,c(1,attr)]
attr <- unique(which(!is.na(M1A[which(numna==45)[1],])))
M1A_45 <- M1A[numna==45,c(1,attr)]
 

# print(sum(is.na(M1A_68))) 0
# print(sum(is.na(M1A_10))) 0


#numna <- vector(length=nrow(M1B))
#for(i in 1:nrow(M1B)){
#        numna[i] <- sum(is.na(M1B[i,]))
#}
# print(unique(numna)) 10 67
#attr <- unique(which(!is.na(M1B[which(numna==67)[1],])))
#M1B_67 <- M1B[numna==67,c(1,attr)]
#attr <- unique(which(!is.na(M1B[which(numna==10)[1],])))
#M1B_10 <- M1B[numna==10,c(1,attr)]
 
# print(sum(is.na(M1B_67))) 0
# print(sum(is.na(M1B_10))) 0

#numna <- vector(length=nrow(M2B))
#for(i in 1:nrow(M2B)){
#        numna[i] <- sum(is.na(M2B[i,]))
#}
# print(unique(numna)) 10 67
#attr <- unique(which(!is.na(M2B[which(numna==67)[1],])))
#M2B_67 <- M2B[numna==67,c(1,attr)]
#attr <- unique(which(!is.na(M2B[which(numna==10)[1],])))
#M2B_10 <- M2B[numna==10,c(1,attr)]
 
# print(sum(is.na(M2B_67))) 0
# print(sum(is.na(M2B_10))) 0

M1A_10_u <- unique(M1A_10)

cluster <- dbscan(matrix(M1A_10_u$DateTime,ncol=1),eps=200000,minPts=1)

tsne <- Rtsne(M1A_10_u, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
cols <- sample(length(col_vector),50)

par(mfrow=c(1,2)) 

plot(M1A_10_u$DateTime,col=cols[cluster$cluster],main="Time vs Row Order of MPDE1A-68 Depublicated Observations (Colored by Time Clustering)",ylab="Posix Time",xlab="Row Order")

plot(tsne$Y,col=cols[cluster$cluster],xlab="",ylab="",main="T-SNE 2-D Embedding of MPDE1A-68 Depublicated Observations")







