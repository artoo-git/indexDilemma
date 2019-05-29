indexDilemma <- function(x, self = 1, ideal = ncol(x), 
                         diff.mode = 1, diff.congruent = NA,
                         diff.discrepant = NA, diff.poles=1, 
                         r.min=.34, exclude=FALSE, digits=2, show=F,
                         output=1, 
                         index=T, trim=20) # CHANGE: set 'self' and
                                  # 'ideal' to first and last column
                                  # respectively
{

  # automatic selection of a priori criteria
  sc <- getScale(x)
  if (is.na(diff.congruent))
    diff.congruent <- floor(diff(sc) * .25)
  if (is.na(diff.discrepant))
    diff.discrepant <-  ceiling(diff(sc) * .6)
  
  # detect dilemmas
  res <- indexDilemmaInternal(x, self=self, ideal=ideal, 
                              diff.mode=diff.mode, diff.congruent=diff.congruent,
                              diff.discrepant=diff.discrepant, diff.poles=diff.poles,
                              r.min=r.min, exclude=exclude, digits=digits, 
                              index=index, trim=trim)
  
  # type of output printed to te console
  enames <- getElementNames2(x, trim = trim, index = T)
  
  if (output == 1) {
    indexDilemmaOut0(res, self, ideal, enames, diff.discrepant, 
                     diff.congruent, exclude, r.min,diff.mode) # added diffmode to print mode in output
    indexDilemmaOut1(res)
    indexDilemmaOut2(res, exclude)
  }
  else if (output == 2) {
    indexDilemmaOut0(res, self, ideal, enames, diff.discrepant, 
                     diff.congruent, exclude, r.min,diff.mode)
    indexDilemmaOut2(res, exclude)
  }
  if (show) 
    indexDilemmaShowCorrelationDistribution(x, self, ideal)
  invisible(res)
}



######################
# Pemutation test to test if grid is random.
# "The null hypothesis [is] that a particular grid 
# is indis- tinguishable from an array of random numbers" 
# (Slater, 1976, p. 129).
#
randomTest <- function(x){
  x
}
# permutationTest
# Hartmann 1992: 
# To illustrate: If a person decided to produce a nonsense grid, 
# the most appropriate way to achieve this goal would be to rate 
#(rank) the elements randomly. The variation of the elements on 
# the con- structs would lack any psychological sense. Every 
# statistical analysis should then lead to noninterpretable results.


# THIS IS FUNCTION IS NEEDED TO OUTPUT DILEMMAS CORRECTLY
get.pole <- function(grid, pole){
  # NEW : get the label of the pole to invert construct if needed
  names <- function(grid){
    grid[[1]]
  }
  poles <- as.data.frame(lapply(grid@constructs, sapply, names))
  colnames(poles)<-1:length(poles)
  poles <- data.frame(lapply(poles, as.character), stringsAsFactors=FALSE)
  
  leftpole <- poles[1,]
  rightpole <- poles[2,]
  
  if (pole == 'left') {
    return(leftpole)
  }
  else if (pole == 'right') {
    return(rightpole)
  }
  else {
    print('Please, introduce left or right pole')
  }
}

indexDilemmaInternal <- function(x, self, ideal, 
                            diff.mode = 1, diff.congruent = 1,
                            diff.discrepant = 4, diff.poles = 1, 
                            r.min, exclude = FALSE, digits = 2,
                            index = T, trim = FALSE) # CHANGE: set defaults
                                                     # to RECORD 5.0 defaults
{
  s <- getRatingLayer(x)         # grid scores matrix
  # NEW: To invert constructs
  # create a vector of inverted scores for the 'self' element:
  # invscr = 8 - scr
  # Example: 2 -> 8 - 2 -> 6
  #          5 -> 8 - 5 -> 3
  inverteds <- getScale(x)[2] - s + 1   # e.g. 8 - 1
  nc <- getNoOfConstructs(x)
  cnames <- getConstructNames2(x, index=index, trim=trim, mode=1, pre="", post=" ")
  
  sc <- getScale(x)
  midpoint <- (sc[1] + sc[2])/2     # NEW (DIEGO) get scale midpoint this is importat in
                                    # when Alejandro's code check whether self/ideal   
                                    # is == to the midpoint or not (see below "Get Dilemmas" section)
  
  # GET IF CONSTRUCTS ARE DISCREPANT, CONGRUENT OR NEITHER    
  
  # difference self - ideal self  
  diff.between <- abs(s[, self] - s[, ideal])
  
  #is.congruent.e = logical() # UPDATE (DIEGO) - Simplified - we dont need two sets of logical characters variables now
  #type.c.elem <- character()
  is.congruent = logical()
  type.c <- character()
 
  # CORRECTION (ALEJANDRO): a construct 
  # can't be congruent if it's 'self' score is 4 (AKA self-
  # disorientation). Neither can be congruent if IDEAL is 4.
  # CORRECTION (Diego): I have just updated this avoid hardcoding the midpoint!!
  if (diff.mode == 1){
    for (i in 1:nc){
      if (s[,self][i] != midpoint){
        if (s[,ideal][i] != midpoint){
          is.congruent[i] <- diff.between[i] <= diff.congruent        
        }
        else{
          is.congruent[i] <- FALSE
        }
      }
      else{
        is.congruent[i] <- FALSE
      }
    }
  
  is.discrepant<- diff.between >= diff.discrepant
  is.neither <- !is.congruent & !is.discrepant
  
  type.c[is.congruent] <- "congruent"
  type.c[is.discrepant] <- "discrepant"
  type.c[is.neither] <- "neither"
  }
  # # difference from poles NOT YET IMPLEMENTED
  # sc <- getScale(x)
  # diff.pole1 <- abs(s[, c(e.self, e.ideal)] - sc[1])
  # diff.pole2 <- abs(s[, c(e.self, e.ideal)] - sc[2])
  # #are both elements within the allowed distance from the poles and at the same pole (congruent)
  # is.congruent.p <- diff.pole1[,1] <= diff.poles & diff.pole1[,2] <= diff.poles |
  #                   diff.pole2[,1] <= diff.poles & diff.pole2[,2] <= diff.poles
  # is.discrepant.p <- diff.pole1[,1] <= diff.poles & diff.pole2[,2] <= diff.poles |
  #                     diff.pole1[,1] <= diff.poles & diff.pole2[,2] <= diff.poles
  # 
  # is.neither.p <- !is.congruent.p & !is.discrepant.p 
  # type.c.poles[is.congruent.p] <- "congruent"
  # type.c.poles[is.discrepant.p] <- "discrepant"
  # type.c.poles[is.neither.p] <- "neither"
  #
  #
  ####################################################################################
  ## MIDPOINT-BASED CRITERION TO IDENTIFY CONGRUENT AND DISCREPANT constructs 
  ####################################################################################
  #### added by DIEGO
  #   I have tried to implement here the other popular method for the identification of 
  #   Congruent and Discrepant constructs. This proposed below is that applied by IDIOGRID
  #   software (V.2.3)
  #   IDIOGRID uses "the scale midpoint as the 'dividing line' for discrepancies; for example, 
  #   if the actual self (the Subject Element) is rated above the scale midpoint and the ideal 
  #   self (the Target Element) is rated below the midpoint, then a discrepancy exists (and 
  #   vice versa). If the two selves are rated on the same side of the scale or if either 
  #   the actual self or the ideal self are rated at the midpoint of the scale, then a discre- 
  #   pancy does not exist." ( from IDIOGRID manual)

  else if (diff.mode == 0){
    
    is.congruent <- (s[, self] < midpoint  &  s[, ideal] < midpoint) | 
                    (s[, self] > midpoint  &  s[, ideal] > midpoint)
  
    is.discrepant<- (s[, self] < midpoint  &  s[, ideal] > midpoint) | 
                    (s[, self] > midpoint  &  s[, ideal] < midpoint)
  
    is.neither<- !is.congruent & !is.discrepant
    type.c[is.congruent] <- "congruent"
    type.c[is.discrepant] <- "discrepant"
    type.c[is.neither] <- "neither"
  }else {stop("\nNO differentiation method (diff.mode) SELECTED! quitting ..")}

  ############## END OF MIDPOINT-BASED CRITERION ####################################
  



  ####################################################################################
  # DIEGO: This that I have commented-out is now redundant as the variables are not duplicates 
  # anymore and are calculated only in their conditional loop. This is more efficient
  # ###################################################################################
  #  if (diff.mode == 1){
  #  is.congruent <- is.congruent.e
  #  is.discrepant <- is.discrepant.e
  #  type.construct <- type.c.elem
  #  } else if (diff.mode == 0){ ##### ADDED CHOICE "0" for MIDPOINT RATING CRITERION
  #  is.congruent <- is.congruent.p
  #  is.discrepant <- is.discrepant.p
  #  type.construct <- type.c.poles 
  #  }
  # we just need the next line to reconnect with the original indexdilemma routine
  #####################################################################################
  type.construct <- type.c
  # GET CORRELATIONS
  
  # inter-construct correlations including and excluding 
  # the elements self and ideal self
  rc.include <- constructCor(x)                     # TODO digits=digits
  rc.exclude <- constructCor(x[, -c(self, ideal)])  #digits=digits
  
  # correlations to use for evaluation
  if (exclude)
    rc.use <- rc.exclude else
      rc.use <- rc.include
  
  # type.c.poles <- type.c.elem <- rep(NA, nrow(s)) # set up results vectors
  type.c <- rep(NA, nrow(s))
  # GET DILEMMAS
  
  # which pairs of absolute construct correlations are bigger than r.min?
  
  comb <- t(combn(nc, 2)) # all possible correlation pairs (don't repeat)
  needs.to.invert <- logical()
  
  # set up result vectors
  check <- bigger.rmin <- r.include <- r.exclude <- 
    type.c1 <- type.c2 <- rep(NA, nrow(comb))
  
  # check every pair of constructs for characteristics
  for (i in 1:nrow(comb)){
    c1 <-	comb[i,1]
    c2 <- comb[i,2]
    r.include[i] <- rc.include[c1, c2]
    r.exclude[i] <- rc.exclude[c1, c2]
    type.c1[i] <- type.construct[c1]
    type.c2[i] <- type.construct[c2]
    
    # CORRECTION:
    # To create a dilemma, the 'self' scores of both contructs must be
    # on the same pole. We have to check for that.
    
    # REMOVED HARDCODED MIDPOINT
    # DIEGO: 4 is the midpoint and it was "hardcoded". This is not good if we have a scoring range
    # that is not 1-7 because in that case the midpoint will NOT be 4!
    #
    # DIEGO: another bug-fix is that in the section where the scripts "reorient" the constructs:
    # the code to re-orient the constructs is not controlling for self or ideal self to be scored 
    # as the midpoint. This causes the script break. I have added a condition for those combinations 
    # equivalent to self-score != midpoint
    
    if (s[c1, self] != midpoint & s[c2, self] != midpoint){
      if (s[c1, self] > midpoint & s[c2, self] > midpoint) {   
        if (rc.use[c1, c2] >= r.min) # CORRECTION: don't use ABS values,
          # we invert scores to check constructs
          # to find correlations the other way
          bigger.rmin[i] <- TRUE else
            bigger.rmin[i] <- FALSE
          check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
            (is.discrepant[c1] & is.congruent[c2])
          needs.to.invert[c1] <- TRUE
          needs.to.invert[c2] <- TRUE
      }
      else if (s[c1, self] < midpoint & s[c2, self] < midpoint) {
        if (rc.use[c1, c2] >= r.min)
          bigger.rmin[i] <- TRUE else
            bigger.rmin[i] <- FALSE
          check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
            (is.discrepant[c1] & is.congruent[c2])
          needs.to.invert[c1] <- FALSE
          needs.to.invert[c2] <- FALSE
      }
      
      # NEW:
      # Now check for inverted scores.
      # You only need to invert one construct at a time
      
      if (inverteds[c1, self] > midpoint & s[c2, self] > midpoint) {
        r.include[i] = cor(inverteds[c1,], s[c2,])
        r.exclude[i] = "*Not implemented"
        if (r.include[i] >= r.min) 
          bigger.rmin[i] <- TRUE else
            bigger.rmin[i] <- FALSE
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c2] <- TRUE
      }
      else if (inverteds[c1, self] < midpoint & s[c2, self] < midpoint){
        r.include[i] = cor(inverteds[c1,], s[c2,])
        r.exclude[i] = "*Not implemented"
        if (r.include[i] >= r.min) 
          bigger.rmin[i] <- TRUE else
            bigger.rmin[i] <- FALSE
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c1] <- TRUE
      }
      
      if (s[c1, self] > midpoint & inverteds[c2, self] > midpoint) {
        r.include[i] = cor(s[c1,], inverteds[c2,])
        r.exclude[i] = "*Not implemented"
        if (r.include[i] >= r.min) 
          bigger.rmin[i] <- TRUE else
            bigger.rmin[i] <- FALSE
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c1] <- TRUE
      }
      else if (s[c1, self] < midpoint & inverteds[c2, self] < midpoint) {
        r.include[i] = cor(s[c1,], inverteds[c2,])
        r.exclude[i] = "*Not implemented"
        if (r.include[i] >= r.min) 
          bigger.rmin[i] <- TRUE else
            bigger.rmin[i] <- FALSE
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c2] <- TRUE
      }
    }
    else{ # DIEGO: closing of the if() where I put the condition for self to be != to the midpoint score
    needs.to.invert[c1] <- FALSE
    needs.to.invert[c2] <- FALSE
    }
    #print(paste(needs.to.invert,s[c1,self],s[c2,self])) # Diego debug printout of variables
  }
  # New: invert construct label poles if needed
  needs.to.invert[is.na(needs.to.invert)] <- F
  #print(needs.to.invert)
  #print(nc)
  #print(is.na(needs.to.invert))
  
  leftpole <- get.pole(x, 'left')
  rightpole <- get.pole(x, 'right')
  
  for (i in 1:nc) {
    if (needs.to.invert[i]) {
      s[i, self] <- inverteds[i, self]
      cnames[i] = paste(rightpole[i], leftpole[i], sep = '-')
    }
    else {
      cnames[i] = paste(leftpole[i], rightpole[i], sep = '-')
    }
    
  }
  
  # GET RESULTS
  
  # 1: this data frame contains information related to 'self' and 'ideal' elements
  
  res1 <- data.frame(a.priori=type.construct, self=s[, self], ideal=s[, ideal],
                     stringsAsFactors=F)
  colnames(res1) <- c("A priori", "Self", "Ideal")
  rownames(res1) <- cnames
  
  # 2: This dataframe stores the information for all posible construct combinations
  res2 <- data.frame(c1=comb[,1], c2=comb[,2], r.inc=r.include, 
                     r.exc=r.exclude, bigger.rmin, type.c1, type.c2, check,
                     name.c1=cnames[comb[,1]], name.c2=cnames[comb[,2]], 
                     stringsAsFactors=F) 
  
  # 3: This dataframe contains informartion for all the dilemmas
  res3 <- subset(res2, check==T & bigger.rmin==T)  
  
  cnstr.labels = character()
  cnstr.labels.left <- cnstr.labels.right <- cnstr.labels
  
  # Number of dilemmas
  nd <- length(res3$c1)
  
  # New: Put all discrepant constructs to the right
  if (nd != 0) {
    for (v in 1:nd) {
      if (res3$type.c1[v] == 'discrepant') {
        cnstr.labels.left[v] = res3[v, 10]
        cnstr.labels.right[v] = res3[v, 9]
      }
      else {
        cnstr.labels.left[v] = res3[v, 9]
        cnstr.labels.right[v] = res3[v, 10]
      }
    }
  }
  
  # 4: NEW: reordered dilemma output
  res4 <- data.frame(cnstr.labels.left, Rtot=res3[,3], cnstr.labels.right, RexSI=res3[,4])                
  colnames(res4) = c('Self - Not self', 'Rtot', 'Self - Ideal', 'RexSI')
  
  list(res1=res1, res2=res2, res3=res3, res4=res4)
}
# output function for indexDilemma
#
indexDilemmaOut0 <- function(res, self, ideal, enames, 
                             diff.discrepant, diff.congruent, exclude, r.min, diff.mode){
  cat("\n###################\n")
  cat("Implicative Dilemma")
  cat("\n###################\n")
  
  cat("\nActual Self Position:", enames[self])               
  cat("\nIdeal Self Position:", enames[ideal])   
  
  cat("\n\nA Priori Criteria (for classification):")
  # differentiation mode 0 for midpoint-based criterion (Grimes - Idiogrid) OR
  # differentiation mode 1 for Feixas "correlation cut-off" criterion
  if (diff.mode == 1){
    cat("\nDiscrepant Difference: >=", diff.discrepant)
    cat("\nCongruent Difference: <=", diff.congruent)
  }else if (diff.mode == 0){
    cat("\nUsing Midpoint rating criterion")
  }
  cat("\n\nCorrelation Criterion: >=", r.min)
  if (exclude)
    cat("\nCriterion Correlation excludes Self & Ideal") else 
      cat("\nCriterion Correlation includes Self & Ideal\n")
  
  cat("\nNumber of Implicative Dilemmas found:", nrow(res$res4), "\n")
  #Extreme Criteria:
  #Discrepant Difference: Self-Ideal greater than or equal to, Max Other-Self difference
  #Congruent Difference: Self-Ideal less than or equal to, Min Other-Self difference  
}


# output function for indexDilemma
#
indexDilemmaOut1 <- function(res){
  cat("\n\nClassification of Constructs")
  cat("\n############################\n\n")
  print(res$res1)
  cat("\n\tNote: if Self' score is not 4, left pole corresponds to Self\n")
}


# output function for indexDilemma
#
indexDilemmaOut2 <- function(res, exclude){
  # add asteristic to construct names
  # ids <- res$res3
  #  disc.c1 <- ids$type.c1 == "discrepant"
  #  disc.c2 <- ids$type.c2 == "discrepant"
  #  ids$name.c1[disc.c1] <- paste(ids$name.c1[disc.c1], "*", sep="")
  #  ids$name.c2[disc.c2] <- paste(ids$name.c2[disc.c2], "*", sep="")
  cat("\n\nDilemmatic Self-Ideal Construct Pairs")
  cat("\n#####################################")
  cat("\n\nBy A Priori Criteria:\n\n")
  cat("\n\t", 'Congruents on the left - Discrepants on the right', sep="", "\t\n")
  cat("\n", "\n")
  # df <- data.frame(RexSI=ids[,3], Rtot=ids[,4],
  #                   Constructs=paste(ids[,9], ids[,10], sep=" <==> "))
  df <- res$res4
  if (nrow(df) > 0){
    print(df)
    cat("\n\tRexSI = Correlations excluding Self & ideal")
    cat("\n\tRtot  = Correlations including Self & ideal")
    if (exclude)
      cor.used <- "RexSI" else
        cor.used <- "Rtot"
    cat("\n\t", cor.used, " was used as criterion", sep="")
  } else {
    cat("No implicative dilemmas detected")
  }
}
