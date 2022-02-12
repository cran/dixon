`p2colasr` <- 
function (Z) 
{
    # OBSERVED VALUE
    obs <- Z[1]
    
    #Sampling distribution
    samp.d <- table(Z)/length(Z)
    #length of the sampling distribution (numer of different values)
    no <- length(samp.d)
    
    #Sampling distribution with names = value rank
    # (for easy access to p values)
    samp.d2 <- samp.d
    names(samp.d2) <- 1:no
    
    #  Position of the observed value among the sampling distribution
    obs.o <- which(names(samp.d) == obs)
    #probability of the observed outcome
    p.obs <- samp.d[obs.o]
	
    if (obs >= mean(Z[-1])) {
        #A)  RIGHT ONE-SIDED TEST
        p.ge <- sum(samp.d[obs.o:no])
	
	# B) Select the  "least extreme outcome" among the otcomes  
	#  in the other side of the sampling distribution
	    # rank of the distribution values
	    rank.d<-table(rank(Z)/length(Z))
	    # values in the other side of the distribution
	    other.extrem <- which(names(rank.d) <0.5)
	    # probabilities for values in the other side of the distribution
	    samp.d2.extrem <-  samp.d2[other.extrem]
	    #which ones  have p < p(observed outcome)
	    other <- names(which(samp.d2.extrem < p.obs))
            if (length(other) > 0) {
	       # which one is the least extreme
               least.o <- max(other)
               p.least <- sum(samp.d[1:least.o])
              }
            if (length(other) == 0) p.least <- 0
    }
    
    if (obs < mean(Z[-1])) {
        # LEFT ONE-SIDED TEST
        p.ge <- sum(samp.d[1:obs.o])
	
	rank.d<-table(rank(Z)/length(Z))
	
	
	other.extrem <- which(names(rank.d)>=0.5)
	samp.d2.extrem <-  samp.d2[other.extrem]
	other <- names(which(samp.d2.extrem < p.obs))
        if (length(other) > 0) {
            least.o <- min(other)
            p.least <- sum(samp.d2[least.o:no])
        }
        if (length(other) == 0)   p.least <- 0
    }
    
    # RETURN TWO-SIDED TEST
    return(p.ge + p.least)
}
