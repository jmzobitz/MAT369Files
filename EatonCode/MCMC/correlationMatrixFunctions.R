### Functions to help plot the correlation matrix.  Taken from the R graphics cookbook (pg 114) as well as the pairs help page

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)

  text(0.5, 0.5, txt, cex = cex.cor * r)
  
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.lm <- function(x,y,col=par("col"),bg=NA,pch=par("pch"),cex=1,col.smooth="red",lwd.type=2,...) {
  points(x,y,pch=pch,col=col,bg=bg,cex=cex)
  abline(stats::lm(y~x),col=col.smooth,lwd=lwd.type,...)
}

