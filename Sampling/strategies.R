# Comparing Sampling Strategies
strategies <- function(b = 10000, n = 10){
  N <- 31
  Volume <- trees$Volume
  Girth <- trees$Girth
  ybar <- numeric(0)
  hh <- numeric(0)
  ht <- numeric(0)
  grt <- numeric(0)
  nu <- numeric(0)
  px <- Girth/sum(Girth)
  mu <- mean(Volume)
  pix <- 1 - (1 - px)^n
  par(mfcol = c(3, 1))
  
  # simple random sampling, ybar
  for (k in 1:b){
    s <- sample(1:31, n)
    ybar[k] <- mean(Volume[s])
  }
  hist(ybar, xlim = c(10, 50), col = "grey", add=FALSE, prob = TRUE,
       main = paste("srs, ybar, n = ", n))
  points(mean(Volume), 0, pch=24, bg="black", cex=2)
  print.noquote(paste("true population mean mu = ", mu))
  print.noquote(paste("design=srs","n=",n))
  print.noquote(paste(" est E(est) var(est)
                      E(est-mu)^2"))
  print.noquote(paste(" ybar", mean(ybar),var(ybar),
                      mean((ybar-mu)^2)))
  
  # pps design, ybar and hh
  for (k in 1:b)
  {
    s <- sample(1:31,n,repl=T,prob=px)
    su <- unique(s)
    nu[k] <- length(unique(s))
    ybar[k] <- mean(Volume[s])
    hh[k] <- mean(Volume[s]/px[s])/N
    ht[k] <- sum(Volume[su]/pix[su])/N
    grt[k] <- (sum(Volume[su]/pix[su]))/(sum(1/pix[su]))
  }
  
  # summaries
  print.noquote(paste("design=pps","n=",n, "E(nu)=",
                      mean(nu)))
  print.noquote(paste(" ybar", mean(ybar),var(ybar),
                      mean((ybar-mu)^2)))
  print.noquote(paste(" hh", mean(hh),var(hh),
                      mean((hh-mu)^2)))
  print.noquote(paste(" ht", mean(ht),var(ht),
                        mean((ht-mu)^2)))
  print.noquote(paste(" grt", mean(grt),var(grt),
                      mean((grt-mu)^2)))
  # summary of ybar, pps
  hist(ybar,xlim=c(10,50),col="grey",add=F,prob=T,
       main=paste("pps, ybar, n = ",n))
  points(mean(ybar),0,pch=24,bg="white",cex=1.5)
  points(mean(Volume),0,pch=24,bg="black",cex=1.5)
  
  # summary of hh, pps
  hist(hh,xlim=c(10,50),col="grey",add=F,prob=T,
       main=paste("pps, hh, n = ",n))
  points(mean(hh),0,pch=24,bg="white",cex=1.5)
  points(mean(Volume),0,pch=24,bg="black",cex=1.5)
  
  if (0 > 1)
  {
    # summary of ht, pps
    hist(ht,xlim=c(10,50),col="grey",add=F,prob=T,
         main=paste("pps, ht, n = ",n))
    points(mean(ht),0,pch=24,bg="white",cex=1.5)
    points(mean(Volume),0,pch=24,bg="black",cex=1.5)
    # summary of grt, pps
    hist(grt,xlim=c(10,50),col="grey",add=F,prob=T,
         main=paste("pps, gry, n = ",n))
    points(mean(grt),0,pch=24,bg="white",cex=1.5)
    points(mean(Volume),0,pch=24,bg="black",cex=1.5)
  }

}