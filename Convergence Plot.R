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
    
    if (t < 15){ abline(v = seq(20, from = 0, by = 1), lty = 3, col = "grey") }
    
    else if (t > 15 && t < 50){ # when t (time period) > 15 & < 45
      
      abline(v = seq(100, from = 0, by = 10), lty=3, col="grey") }
    
    else if (t > 50 && t < 100){ # when t (time period) > 50 & < 100
      
      abline(v = seq(100, from = 0, by = 20), lty=3, col="grey") }
    
    par(mar = c(8, 4.1, 4.1, 2.1)) # Define borders of the plot
    
    legend(x = "bottom", inset = c(0, -0.3), legend = colnames(l), horiz = T,
           col = c, lwd = 2, cex = .85, bty = "n", xpd = T)
    
    on.exit(par(par(no.readonly = T))) } # Show legend with names
}
time.pace.plt(x = c(20, 1000), c(0.15, 0.06), countries = c("China", "US"),
              c = c("darkred", "navy")) # Test
