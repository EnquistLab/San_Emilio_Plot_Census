library(geometry)
library(MASS)
inhull <- function(testpts, calpts, hull=convhulln(calpts),
 tol=mean(mean(abs(calpts)))*sqrt(.Machine$double.eps)) 
{
  # Efficient test for points inside a convex hull in n dimensions 
  # 
  #% arguments: (input) 
  #% testpts - nxp array to test, n data points, in p dimensions 
  #% If you have many points to test, it is most efficient to 
  #% call this function once with the entire set. 
  #% 
  #% calpts - mxp array of vertices of the convex hull, as used by 
  #% convhulln. 
  #% 
  #% hull - (OPTIONAL) tessellation (or triangulation) generated by convhulln 
  #% If hull is left empty or not supplied, then it will be 
  #% generated. 
  #% 
  #% tol - (OPTIONAL) tolerance on the tests for inclusion in the 
  #% convex hull. You can think of tol as the distance a point 
  #% may possibly lie outside the hull, and still be perceived 
  #% as on the surface of the hull. Because of numerical slop 
  #% nothing can ever be done exactly here. I might guess a 
  #% semi-intelligent value of tol to be 
  #% 
  #% tol = 1.e-13*mean(abs(calpts(:))) 
  #% 
  #% In higher dimensions, the numerical issues of floating 
  #% point arithmetic will probably suggest a larger value 
  #% of tol. 
  #% 
  # In this R implementation default 
  
  tol=mean(mean(abs(calpts)))*sqrt(.Machine$double.eps) 
  # DEFAULT: tol = 1e-6 
  # 
  # VALUE: Matlab returns a vector of TRUE (inside/on) or FALSE (outside) 
  # This R implementation returns an integer vector of length n 
  # 1 = inside hull 
  # -1 = inside hull 
  # 0 = on hull (to precision indicated by tol) 
  #-------------------------------------------------------- 
  require(geometry, quietly=TRUE) # for convhulln    
  require(MASS, quietly=TRUE) # for Null 
  # ensure arguments are matrices (not data frames) and get sizes 
  calpts <- as.matrix(calpts) 
  testpts <- as.matrix(testpts) 
  p <- dim(calpts)[2] # columns in calpts    
  cx <- dim(testpts)[1] # rows in testpts    
  nt <- dim(hull)[1] # number of simplexes in hull 
  
  # find normal vectors to each simplex 
  nrmls <- matrix(NA, nt, p) # predefine each nrml as NA, degenerate
  
  degenflag <- matrix(TRUE, nt, 1) 
  for (i in 1:nt) 
  { 
    nullsp <- t(Null(t(calpts[hull[i,-1],] - matrix(calpts[hull[i,1],],p-1,p, byrow=TRUE))))
    
    if (dim(nullsp)[1] == 1) 
      { nrmls[i,] <- nullsp
        degenflag[i] <- FALSE}
  } 
  # Warn of degenerate faces, and remove corresponding normals 
  if(length(degenflag[degenflag]) > 0) warning(length(degenflag[degenflag])," degenerate faces in convex hull")
  
  nrmls <- nrmls[!degenflag,] 
  nt <- dim(nrmls)[1] 
  
  # find center point in hull, and any (1st) point in the plane of each simplex
  center = apply(calpts, 2, mean) 
  a <- calpts[hull[!degenflag,1],] 
  # scale normal vectors to unit length and ensure pointing inwards 
  nrmls <- nrmls/matrix(apply(nrmls, 1, function(x) sqrt(sum(x^2))), nt, p)    
  dp <- sign(apply((matrix(center, nt, p, byrow=TRUE)-a) * nrmls, 1, sum))    
  nrmls <- nrmls*matrix(dp, nt, p) 
  # if min across all faces of dot((x - a),nrml) is 
  # +ve then x is inside hull 
  # 0 then x is on hull 
  # -ve then x is outside hull 
  # Instead of dot((x - a),nrml) use dot(x,nrml) - dot(a, nrml) 
  aN <- diag(a %*% t(nrmls)) 
  val <- apply(testpts %*% t(nrmls) - matrix(aN, cx, nt, byrow=TRUE), 1,min) 
  # code values inside 'tol' to zero, return sign as integer 
  val[abs(val) < tol] <- 0 
  as.integer(sign(val)) 
}
