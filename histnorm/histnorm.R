histnorm <- function(data, linecol="black", lineweight=1, main = "default", xlab = "default", ...){
  args <- list(...)
  dataname <- deparse(substitute(data))
  data <- na.omit(data)
  isDensity <- isTRUE(args[['prob']])
  xlimits <- args[['xlim']]
  ylimits <- args[['ylim']]
  if(is.null(main)){
    main <- ""
  }
  if(is.null(xlab)){
    xlab <- ""
  }
  if(main == "default"){
    maintitle <- paste("Histogram of ", dataname)
  }
  else{
    maintitle <- main
  }
  if(xlab == "default"){
    xlabel <- dataname
  }
  else{
    xlabel <- xlab
  }
  if(!isDensity){
    tryCatch({
      x11()
      h <- hist(data, warn.unused = FALSE, ...)
    },
    error=function(cond){
      message(cond)
      dev.off()
    })
    if(!is.null(xlimits)){
      xfit <- seq(xlimits[1], xlimits[2], length = 100)      
    }
    else{
      xfit <- seq(min(h$breaks), max(h$breaks), length = 100)
    }
    yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))*length(data)*diff(h$mids[1:2])
    ymax <- max(c(floor(max(yfit)) + 1, max(h$counts)))
    ylim_specified <- !is.null(args[['ylim']])
    dev.off()
    if(ylim_specified){
      hist(data, main=maintitle, xlab=xlabel,...)
    }
    else{
      ylimits <- c(0, ymax)
      hist(data, main=maintitle, ylim=ylimits, xlab=xlabel,...)    
    }
    lines(xfit, yfit, col=linecol, lwd=lineweight)    
  }
  else{
    ylim_specified <- !is.null(args[['ylim']])
    if(ylim_specified){
      hist(data, main=maintitle, xlab=xlabel,...)
    }
    else{
      tryCatch({
        x11()
        h <- hist(data, ...)
      },
      error=function(cond){
        message(cond)
        dev.off()
      })
      c <- curve(dnorm(x, mean=mean(data), sd=sd(data)), add = TRUE)
      ylimits <- c(0, max(c(c$y, max(h$density))))
      dev.off()
      hist(data, main=maintitle, ylim=ylimits, xlab=xlabel,...)
    }
    curve(dnorm(x, mean=mean(data), sd=sd(data)), col=linecol, lwd=lineweight, add = TRUE)
  }
}