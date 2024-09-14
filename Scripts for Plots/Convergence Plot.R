time.pace.plt <- function(x, y, countries, c){ # Plot Convergence
  
  if (y[1] == y[2] || y[1] <= y[2]){ print("Convergence will not happen") }
  
  else { # If rates the same, show message, otherwise calculate and show plot
    
    t <- round(log(x[1] / x[2]) / log((1 + y[2]) / (1 + y[1])), 0)
    
    l <- NULL # Store values
    
    for (n in 1:t){ l <- rbind(l, cbind(x[1]*(1 + y[1])^n,x[2]*(1 + y[2])^n)) }
    
    colnames(l) <- countries # Country names
    rownames(l) <- seq(nrow(l)) # Years
    
    plot(l[,1], type = "l", las = 1, xlab = "Years", ylab = "GDP",
         main = "Convergence of GDP Growth of Two Countries", col = c[1])
    
    lines(l[,2], type = "l", col = c[2]) # Show second line
    
    axis(side = 2, las = 2) # Rotate y-axis number 90 degrees left
    
    abline(h = 0) # Add vertical lines: when t (time period) < 15
    
    m <- round(t) / 10 ^ nchar(round(t))
    
    GDP <- round(max(l[,1])) / 10 ^ nchar(round(max(l[,1])))
    
    i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
    
    for (n in 1:length(i) - 1){ if (m > i[n] && m < i[n + 1]){
      
        mn <- i[n + 1] * 10 ^ (nchar(m) - 3) } else { next }
      
      if (GDP > i[n] && GDP < i[n + 1]){ # Define intervals for GDP and years
        
        mx <- i[n + 1] / 2 * 10 ^ (nchar(GDP) - 3) } else { next } }
    
    abline(v = seq(from = 0, to = t, by = mn), col = "grey", lty = 3) 
    abline(h = seq(from = mx, to = max(l), by = mx), col = "grey", lty = 3) 
    
    par(mar = c(8, 4.1, 5.4, 2.1)) # Define borders of the plot
    
    legend(x = "bottom", inset = c(0, -0.6), legend = colnames(l), horiz = T,
           col = c, lwd = 2, cex = .85, bty = "n", xpd = T)
    
    on.exit(par(par(no.readonly = T))) } # Show legend with names
}
time.pace.plt(x = c(20, 1000), c(0.15, 0.06), countries = c("China", "US"),
              c = c("darkred", "navy")) # Test
