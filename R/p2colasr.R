`p2colasr` <-
function (Z) {
 #observed value
  obs <- Z[1]
  # sampling distribution
  samp.d <- table(Z)/length(Z)
  # number of possible outcomes
  no <- length(samp.d)
  
  # rank of the observed outcome
  obs.o <- which(names(samp.d)==obs)
  
  #probability of the observed outcome 
       p.obs <- samp.d[obs.o]
  
    if (obs > mean(Z[-1])) {
       # tail probability for the observed outcome 
       p.ge <- sum(samp.d[obs.o : no])
  
       # which outcomes  in the  "other" tail  are less probable than the observed outcome
       other <- which(samp.d [1: (obs.o -1)] < p.obs)
  
       # which is the least extreme outcome in the  "other" tail  that is less probable than the observed outcome
       if(length(other)>0) {
          least.o <-   max(which(samp.d [1: (obs.o -1)] < p.obs))
          # probability for the least extreme outcome
          p.least <- samp.d [least.o] 
       }
       # if none is "less", probable, p. least = 0
       if(length(other)==0)  p.least <- 0
    }
    if (obs < mean(Z[-1])) {
       # tail probability for the observed outcome 
       p.ge <- sum(samp.d[1:obs.o])
  
       # which outcomes  in the  "other" tail  are less probable than the observed outcome
       other <- which(samp.d [(obs.o +1):no] < p.obs)
  
       # which is the least extreme outcome in the  "other" tail  that is less probable than the observed outcome
       if(length(other)>0) {
          least.o <-   min(which(samp.d [(obs.o +1):no] < p.obs))
          # probability for the least extreme outcome
         p.least <- samp.d [least.o] 
       }
       # if none is "less", probable, p. least = 0
       if(length(other)==0)  p.least <- 0
   }
    
   # 2-sided p-value
   return( p.ge + p.least)
         
}


