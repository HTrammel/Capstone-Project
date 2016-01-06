# MARKOV CHAIN LESSON
# Snakes and Ladders

n <- 100
M <- matrix(0,n+1,n+1+6)
rownames(M) <- 0:n
colnames(M) <- 0:(n+6)

for (i in 1:6) {
    diag(M[,(i+1):(i+1+n)]) <- 1/6
}

M[,n+1] <- apply(M[, (n+1):(n+1+6)], 1, sum)

M <- M[,1:(n+1)]

starting <- c(4,9,17,20,28,40,51,54,62,64,63,71,93,95,92)
ending  <- c(14,31,7,38,84,59,67,34,19,60,81,91,73,75,78)

for(i in 1:length(starting)){
	v <- M[,starting[i]+1]
	ind <- which(v>0)
	M[ind,starting[i]+1] <- 0
	M[ind,ending[i]+1] <- M[ind,ending[i]+1]+v[ind]
}



powermat<- function(P,h){
	Ph<- P
	if (h>1) {
    	for(k in 2:h) {
    	    Ph<- Ph%*%P
	    }
    }
	return(Ph)
}

initial <- c(1,rep(0,n))

COLOR <- rev(heat.colors(101))

u <- 1:sqrt(n)

boxes <- data.frame(
	index <- 1:n,
	ord <- rep(u,each<- sqrt(n)),
	abs <- rep(c(u,rev(u)),sqrt(n)/2)
	)

position<- function(h = 1){
	D <- initial%*%powermat(M,h)
	plot(0:10,
		0:10,
		col<- "white",
		axes<- FALSE,
		xlab<- "",
		ylab<- "",
		main<- paste("Position after", h, "turns"))
	segments(0:10, rep(0,11), 0:10, rep(10,11))
	segments(rep(0,11), 0:10, rep(10,11), 0:10)

	for(i in 1:n){
		polygon(boxes$abs[i]-c(0,0,1,1),
				boxes$ord[i]-c(0,1,1,0),
				col<- COLOR[min(1+trunc(500*D[i+1]),101)],
				border<- NA)
		}

	text(boxes$abs-.5,
		 boxes$ord-.5,
		 boxes$index,
		 cex<- .7)

	segments(c(0,10),rep(0,2),c(0,10),rep(10,2))
	segments(rep(0,2),c(0,10),rep(10,2),c(0,10))

}