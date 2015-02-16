obtenerPosicionOutcome <- function(outcome) {
  HEART_ATTACK <- list("heart attack", 11)
  
  HEART_FAILURE <- list("heart failure", 17)
  
  PNEUOMONIA <- list("pneumonia", 23)
  
  if(identical(HEART_ATTACK[[1]],outcome)) {
    return(HEART_ATTACK[[2]])
  } else if(identical(HEART_FAILURE[[1]],outcome)) {
    return(HEART_FAILURE[[2]])
  } else if(identical(PNEUOMONIA[[1]],outcome)) {
    return(PNEUOMONIA[[2]])
  } else {
    return(NULL)
  }
  
}
rankall <- function(outcome, num = "best") {
  
  nNum <- numeric()
  
  hospital <- c()
  
  state <- c()
  
  posOutcome <- obtenerPosicionOutcome(outcome)
  
  if(is.null(posOutcome)) {
    stop("invalid outcome")
    return
  }
  
  if(is.character(num)) {
    if(!identical(num, "best") & !identical(num, "worst")){
      stop("invalid num")
    }
  } else if(!is.numeric(num)) {
    stop("invalid num")
  }
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  estados <- levels(factor(outcome[["State"]]))
  
  outcome[,posOutcome] <- as.numeric(outcome[,posOutcome,])
  
  separado <- split(outcome, estados)
  
  separado <- na.omit(separado)
  
  if(identical(num, "best")) {
    nNum <- 1
  } else {
    nNum <- num
  }
  
  for(estado in estados) {
    
    bloque <- get(estado, separado)
    
    if(identical(num, "worst")) {
      nNum <- nrow(bloque)
    }
  
    ordenado <- bloque[order(bloque[[posOutcome]], bloque[[2]]),]
    
    print(ordenado)
  
    hospital <- c(hospital, ordenado[nNum,2])
    
    state <- c(state, estado)
  }
  
  data.frame(hospital, state)
}