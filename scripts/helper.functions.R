
mult.rast <- function(r="",incl=0){
  r1 <- unstack(r)
  r1 <- r1[incl]
  if (length(r1) >1){
    r2 <- r1[[1]] * r1[[2]]
    
    for (ii in 3:length(r1)){
      r2 <- r2 * r1[[ii]]
    }
    val <- getValues(r2)
    r3 <- r2 / max(val,na.rm=T)
    return(r3)
  } else {
    return(r1)
  }
  
}

add.rast <- function(r="",incl=0){
  r1 <- unstack(r)
  r1 <- r1[incl]
  if (length(r1) >1){
    r2 <- r1[[1]] + r1[[2]]

    for (ii in 3:length(r1)){
      r2 <- r2 + r1[[ii]]
    }
    val <- getValues(r2)
    r3 <- r2 / sum(incl)
    return(r3)
    } else {
    return(r1)
  }

}
