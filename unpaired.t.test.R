## Unpaired t test (only with sample size, mean, and standard error)
t_unpair <- function(n1,m1,sd1,n2,m2,sd2,tail){
  
  sp2 <- ((n1-1)*sd1^2 + (n2-1)*sd2^2)/((n1-1)+(n2-1))
  se <- sqrt(sp2/n1 + sp2/n2)
  t <- (m1-m2)/se 
  
  if(tail == "two" & m1 > m2 ){
    sig_t <- qt(p = 0.025, df = min(n1, n2) - 1, lower.tail = FALSE)
    if (t < sig_t){
      reject <- "False"
    } else if (t > sig_t){
      reject <- "True"
    }
  } else if(tail == "two" & m1 < m2){
    sig_t <- qt(p = 0.025, df = min(n1, n2) - 1, lower.tail = TRUE)
    if (t<sig_t){
      reject <- "True"
    } else if (t>sig_t){
      reject <- "False"
    }
  } else if(tail == "one" & m1 > m2){
    sig_t <- qt(p = 0.05, df = min(n1, n2) -1, lower.tail = FALSE)
    if (t<sig_t){
      reject <- "False"
    } else if (t>sig_t){
      reject <- "True"
    }
  } else if (tail == "one" & m1<m2){
    sig_t <- qt(p = 0.05, df = min(n1,n2) -1, lower.tail = TRUE)
    if (t<sig_t){
      reject <- "True"
    } else if (t>sig_t){
      reject <- "False"
    }
  }
  
  my_list <- list(sp2,se,t,reject)
  names(my_list) <- c("sp2", "se", "t", "reject")
  print(my_list)
  
  x <- seq(-3,3,by=0.1)
  dnorm <- dnorm(x)
  plot(x,dnorm,type="l", ylab="density")
  abline(v=t, col = 'red')
  abline(v=sig_t, col = 'red', lty = 'dashed')
  legend("topright", legend = c("Solid = t", "Dashed = sig_t"), cex = 0.7)
}