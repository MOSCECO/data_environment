evplot <- function(ev) {  
  # Broken stick model (MacArthur 1957)  
  n <- length(ev)  
  bsm <- data.frame(j=seq(1:n), p=0)  
  bsm$p[1] <- 1/n  
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))  
  bsm$p <- 100*bsm$p/n  
  # Plot eigenvalues and % of variation for each axis  
  op <- par(mfrow = c(2,1),omi = c(0.1,0.3,0.1,0.1), mar = c(1, 1, 1, 1))  
  barplot(ev, main = paste("Eigenvalues"), col = "bisque", las = 2)  
  abline(h = mean(ev), col = "red")  
  legend("topright", "Average eigenvalue", lwd = 1, col = 2, bty = "n")  
  barplot(
    t(cbind(100*ev/sum(ev), bsm$p[n:1])), 
    beside=TRUE,   
    main= paste("% variation"), 
    col=c("bisque",2), 
    las=2
  )  
  legend(
    "topright", 
    c("% eigenvalue", "Broken stick model"),   
    pch=15, 
    col=c("bisque", 2), 
    bty="n"
  )  
  par(op)  
} 
