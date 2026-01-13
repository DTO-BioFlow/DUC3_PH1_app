# set working directory to current folder
# Menu -- Session -- Set Working Directory -- To Source File Location
# This script is saved in the same folder as the data to load -- if not, change the path to tthe file in the "read.xlsx" command
# Load raw data -- monthly





#######################################################################################
# Function definitions
#######################################################################################
# -------------------------------------------------------------------------------------
## trimmed width function
# -------------------------------------------------------------------------------------
# the lines below need to be run once to load the functions into the workspace
# if anyhting is changed, run again to update functions in workspace
# the functions are needed to perform a resampling procedure, e.g.


trimmedwidth <- function(x,p=1){
  if (p >= 1){p = 0.9} # correct wrong input for skip fraction
  if (p < 0.5){p = 0.5} # same as above
  plex <- (1-p)/2
  lx <- length(x)
  lr <- (max(x) - min(x)) / 1000
  lp <- (rnorm(lx,1) - 0.5)*lr
  # add perturbation to vector
  x = x + lp
  upperindex = ceiling(lx*(1-plex))
  lowerindex = ceiling(lx*plex)
  sortedx <- sort(x)
  upperx <- sortedx[upperindex]
  lowerx <- sortedx[lowerindex]
  iv <- !(x<=lowerx | x>=upperx)
  iv
}



# -------------------------------------------------------------------------------------
## create convex hull -- inner and outer envelope
# -------------------------------------------------------------------------------------


findEvn <- function(x,y,p=0.9,sc=TRUE){
  # input:
  # x,y species, transformed, output from extractdata e.g
  # p : fraction to be excluded, numeric between 0 and 1 with 0, all points are excluded, 1, all points are included
  # upper - p 
  # lower - 1-p
  # sc : scaling data, logical, true or anything else than true (means then false)
  # data for envelope calculation is scaled by the std in both directions, x and y
  
  if (p > 1){
    pWrong = p
    p = 0.999
    fprintf("p value was corrected from %.2f to %.2f",pWrong,p)
  } # correct wrong input for skip fraction
  if (p < 0.5){
    pWrong = p
    p = 0.51
    fprintf("p value was corrected from %.2f to %.2f",pWrong,p)
  } # same as above
  
  medX <- median(x)
  rX <- sd(x)
  medY <- median(y)
  rY <- sd(y)  
  
  # scaling by std x and std y if sc is true
  if(sc==TRUE){
    # scale by std
    xSC <- (x-medX) / rX
    ySC <- (y-medY) / rY
    # get polar coordinates
    ang <- atan2(xSC,ySC)
    vec <- sqrt(abs(xSC)**2+abs(ySC)**2)
  } else{
    # get polar coordinates
    ang <- atan2(x-medX,y-medY)
    vec <- sqrt(abs(x-medX)**2+abs(y-medY)**2)
  }
  
  # replace zero lengths by small number to avoid dividing by 0
  vec <- replace(vec, vec==0, 0.01)
  # trimm data to p lower and 1-p upper 
  tb <- trimmedwidth(vec,p)
  x <- x[tb]
  y <- y[tb]
  ang <- ang[tb]
  vec <- vec[tb]
  
  # inverse of norm for inner envelope
  inv1 <- 1/vec
  invx2 <- inv1*cos(ang) 
  invy2 <- inv1*sin(ang) 
  
  # inner and outer envelopes by chull
  idxEnvOut <- chull(x,y)
  idxEnvIn <- chull(invx2,invy2)
  
  # future options - expand to n-dimensions -> convhulln() and inhulln()
  ptsEnvOutX <- x[c(idxEnvOut,idxEnvOut[1])]
  ptsEnvOuty <- y[c(idxEnvOut,idxEnvOut[1])]
  
  ptsEnvInX <- x[c(idxEnvIn,idxEnvIn[1])]
  ptsEnvIny <- y[c(idxEnvIn,idxEnvIn[1])]
  
  envelopePts <- list("EnvOuter"=data.frame("outX" = ptsEnvOutX,"outY"=ptsEnvOuty),
                      "EnvInner"=data.frame("inX" = ptsEnvInX,"inY" = ptsEnvIny))
  return(envelopePts)
}


# -------------------------------------------------------------------------------------
## calculate PIndex
# -------------------------------------------------------------------------------------


PIcalc <- function(testData,envPTS,p){
  # input: data to be tesed as output from extract data
  # envPTS: inner and outer envelope - output from findEnv
  # fraction to be excluded, given in sourceFile.xlsx threshVal
  xN <- testData$y1 # new data - data to be tested, species 1
  yN <- testData$y2 # species 2
  
  # inner and outer envelope
  xpOut <- envPTS$EnvOuter$outX 
  ypOut <- envPTS$EnvOuter$outY
  
  xpIn <- envPTS$EnvInner$inX
  ypIn <- envPTS$EnvInner$inY
  
  # find points that are located inside the outer envelope
  inOuterPoly <- inpolygon(xN,yN,xpOut,ypOut,boundary = FALSE)
  # find points that fall into the inner envelope
  inInnerPoly <- inpolygon(xN,yN,xpIn,ypIn,boundary = FALSE)
  
  ## data should fall in outer polygon but not in innner polygone
  newPts <- inOuterPoly & !inInnerPoly
  
  # PI number 
  nTotalNew <- length(newPts)
  nInsideNew <- sum(newPts, na.rm = TRUE)
  notInsideNew <- nTotalNew - nInsideNew
  
  PI <- nInsideNew/nTotalNew 
  
  ## statistics
  if(p>=1){p <- 0.99}
  if(p<0.5){p <- 0.51}
  q = 1-p
  outcomes <- zeros(nTotalNew,1) # preallocate
  
  for (k in (0:nTotalNew)) {
    for (j in nTotalNew-k) {
      outcomes[k+1] <- nchoosek(nTotalNew,k) * p^j * q^k
    }
  }
  idxKeep <- (notInsideNew):(nTotalNew)
  BinProb <- zeros(length(idxKeep),1)
  BinProb[1:length(idxKeep)] <- outcomes[idxKeep]
  
  BinProb <- sum(BinProb)
  
  EIn = nTotalNew * p # fraction that was kept
  EOut = nTotalNew * q # fraction that was removed
  
  # chi squared
  chisquare <- (nInsideNew - EIn)^2/EIn + (notInsideNew - EOut)^2/EOut
  
  output <- list("PI" = PI, 
                 "newPoints"= nTotalNew, 
                 "pointsInside" = nInsideNew, 
                 "binomial probability" = BinProb, 
                 "chi.sq" = chisquare)
  return(output)
}



#######################################################################################
# End of function definitions
#######################################################################################
