time.pace.plt <- function(x, y){ # Plot Convergence of Two GDP growths
  
  t <- round(log(x[1]/x[2]) / log((1 + y[2])/(1 + y[1])), 0)
  
  l <- NULL
  
  for (n in 1:t){ l <- rbind(l, cbind(x[1]*(1 + y[1])^n, x[2]*(1 + y[2])^n)) }
  
  colnames(l) <- c("China", "US")
  rownames(l) <- seq(nrow(l))
  
  plot(l[,1], type = "l", las = 1, xlab = "Years", ylab = "GDP",
       main = "Convergence of GDP Growth of Two Countries", col = "darkred")
  
  lines(l[,2], type = "l", col = "navy")
  
  axis(side = 2, las = 2)
  
  abline(h = 0)
  
  if (t < 15){ abline(v = seq(20, from = 0, by = 1), lty = 3, col = "grey") }
  
  else if (t > 15 && t < 50){
    
    abline(v = seq(100, from = 0, by = 10), lty=3, col="grey") }
  
  else if (t > 50 && t < 100){
    
    abline(v = seq(100, from = 0, by = 20), lty=3, col="grey") }
}
time.pace.plt(x = c(20, 1000), c(0.15, 0.06))
