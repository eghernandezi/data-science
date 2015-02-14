obtenerPosicionOutcome <- function(outcome) {
    HEART_ATTACK <- list("heart attack", 31)
    
    HEART_FAILURE <- list("heart failure", 19)
    
    PNEUOMONIA <- list("pneumonia", 43)
    
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

best <- function(state, outcome) {
    
    posOutcome <- obtenerPosicionOutcome(outcome)
    
    if(is.null(posOutcome)) {
        print("El parámetro outcome contiene un valor inválido")
        return
    }
        
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    estados <- levels(factor(outcome[["State"]]))
    
    if(!is.element(state,estados)) {
        print("El parámetro estado contiene un valor inválido")
    }
    
    filtrado <- subset(outcome, State == state)
    
    filtrado[,posOutcome] <- as.numeric(filtrado[,posOutcome,])
    
    na.omit(filtrado)
    
    ordenado <- filtrado[order(filtrado[[posOutcome]], filtrado[[2]]),]

    ordenado[1,2]
    
}
