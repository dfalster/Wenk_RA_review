blank_plot <- function(xlim = c(0.02, 1.3), ylim = c(0.04, 15), use.box=TRUE,...){
  plot.new()
  plot.window( xlim = xlim, ylim = ylim, xaxt='n', yaxt='n', ann=FALSE,...)
  if(use.box)
    box()
}


to.dev <- function(expr, dev, filename, ..., verbose=TRUE){
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

na.clean <-function(x){x[!is.na(x)]}

#returns vector from lo to hi with multiplication steps of incr. Used for making ticks to a log-scaled axis
seq.log <- function(from, to, base)
{base^(log(from,base):log(to,base))}

#returns vector of minor tick spacings approrpaite for log 10 scaled axis with major ticks given by 'major'.
seq.log.minor<-function(major)
{
  temp<-NULL;
  if(length(major) > 1)
    for (i in 1:(length(major)-1))
      temp<-c(temp, seq(major[i], major[i+1]-major[i], major[i]));
  temp<-c(temp, major[length(major)]);
  return (temp);
}



#returns up to 80 unique, nice colors, generated using http://tools.medialab.sciences-po.fr/iwanthue/
# Starts repeating after 80
niceColors<-function(n=80){
  cols<-rep(c("#75954F","#D455E9","#E34423","#4CAAE1","#451431","#5DE737","#DC9B94","#DC3788","#E0A732","#67D4C1","#5F75E2","#1A3125","#65E689","#A8313C","#8D6F96","#5F3819","#D8CFE4","#BDE640","#DAD799","#D981DD","#61AD34","#B8784B","#892870","#445662","#493670","#3CA374","#E56C7F","#5F978F","#BAE684","#DB732A","#7148A8","#867927","#918C68","#98A730","#DDA5D2","#456C9C","#2B5024","#E4D742","#D3CAB6","#946661","#9B66E3","#AA3BA2","#A98FE1","#9AD3E8","#5F8FE0","#DF3565","#D5AC81","#6AE4AE","#652326","#575640","#2D6659","#26294A","#DA66AB","#E24849","#4A58A3","#9F3A59","#71E764","#CF7A99","#3B7A24","#AA9FA9","#DD39C0","#604458","#C7C568","#98A6DA","#DDAB5F","#96341B","#AED9A8","#55DBE7","#57B15C","#B9E0D5","#638294","#D16F5E","#504E1A","#342724","#64916A","#975EA8","#9D641E","#59A2BB","#7A3660","#64C32A"),
            ceiling(n/80))
  cols[1:n]
}


make.transparent <- function(col, opacity=0.5) {
  tmp <- col2rgb(col)/255
  rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
}

## Position label at a fractional x/y position on a plot
label <- function(px, py, lab, ..., adj=c(0, 1)) {
  usr <- par("usr")
  x <- usr[1] + px*(usr[2] - usr[1])
  y <- usr[3] + py*(usr[4] - usr[3])

  if(par("ylog"))
    y <- 10^y
  if(par("xlog"))
    x <- 10^x

  text(x,y, lab, adj=adj, ...)
}


is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

axis.log10 <- function(side=1, horiz=FALSE, labels=TRUE, baseAxis = TRUE, wholenumbers=T, labelEnds=T,las=1, at=NULL) {

  fg <- par("fg")

  if(is.null(at)){

    #get range on axis
    if(side ==1 | side ==3) {
      r <- par("usr")[1:2]   #upper and lower limits of x-axis
    } else {
      r <- par("usr")[3:4] #upper and lower limits of y-axis
    }

    #make pertty intervals
    at <- pretty(r)
    #drop ends if desirbale
    if(!labelEnds)
      at <- at[at > r[1] & at < r[2]]
  }
  #restrict to whole numbers if desriable
  if(wholenumbers)
    at<-at[is.wholenumber(at)]

  lab <- do.call(expression, lapply(at, function(i) bquote(10^.(i))))

  #convert at if
  if(baseAxis)
    at<-10^at

  #make labels
  if ( labels )
    axis(side, at=at, lab, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
  else
    axis(side, at=at, FALSE, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
}






# update version of plot.sma, Makes lines on plot trasnparent, with level set according to r2.
# line.transp.p sets, level at which line is fully opaque, set to large number (e.g. 1E3) for no transparency (default = 0.05)

plot.sma <- function(x, which=c("default","residual","qq"),  use.null=FALSE, add=FALSE, type='o',
                     xaxis=NULL, yaxis=NULL, xlab=NULL, ylab=NULL, pch=NULL, col=NULL, lty=NULL, from=NULL, to = NULL, log=x$log,
                     frame.plot = TRUE, tck=par("tck"),
                     line.transp.p=0.05,...){

  #preprocessing ------------------------------------------------------------
  obj <- x  # this needed for consistency with plot
  if(obj$gt == "none"){
    ngrps <- 1
  }
  else{
    groups <- levels(obj$data[,3])
    ngrps <- length(groups)
  }

  whichplot <- match.arg(which)

  #---colors--------------------------------
  #user-defined colors
  if(!is.null(col[1])){
    if(length(col)== 1 &&  ngrps > 1)
      col<-rep(col[1],ngrps); #check right vector length
  } else {
    #default colors
    col <- c("blue2",  "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1",
             "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3",
             "goldenrod4", "darkolivegreen2", "palevioletred3", "darkseagreen3", "sandybrown", "tan",
             "gold", "violetred4", "darkgreen")
    col <- rep(col, ceiling(ngrps/length(col)))
  }

  #---symbols--------------------------------
  #user-defined symbols
  if(!is.null(pch[1])){
    if(length(pch) == 1 && ngrps > 1)
      pch <- rep(pch[1],ngrps) #check right vector length
  } else #default SYMBOLS
    pch <- rep(1,ngrps)
  #---line type--------------------------------  #user-defined symbol
  if(!is.null(lty[1])){
    if(length(lty) == 1 && ngrps > 1)
      lty<-rep(lty[1],ngrps) #check right vector length
  } else #default SYMBOLS
    lty <- rep("solid", ngrps)
  #-----------------------------------------------------------------------------
  # DATA PLOT
  if(whichplot == "default"){

    #obtain data--------------------------------
    Y <- obj$data[,1]
    X <- obj$data[,2]

    #log trasnformations--------------------------------
    log_data <- obj$log	#logging of data based on transformation applied in sma. Allows scaling of axes ot be changed, while maintaing correct transformation of data
    XLog <- YLog <- 0
    if((log_data == "x") | (log_data == "xy")){ XLog=1; X = 10^X}
    if((log_data == "y") | (log_data == "xy")){ YLog=1; Y = 10^Y}

    #axis labels--------------------------------
    #determine axis labels if not already specified
    if(is.null(xlab)){
      xlab <- names(obj$data)[2]
      if(XLog)
        xlab <- paste(xlab, "[log scale]")
    }
    if(is.null(ylab)){
      ylab <- names(obj$data)[1]
      if(YLog)
        ylab <- paste(ylab, "[log scale]")
    }

    #SETUP AXES--------------------------------
    if(add==FALSE)
    {
      #use nice plotting if appropriate
      if(!is.null(xaxis)  && !is.null(yaxis)){

        #Deteremine axis limits if missing - caluclated on transformed data. 				#add 5% white space around plots, then back transform if necessary
        if (is.null(xaxis$limits)){
          Range_x <-range(obj$data[,2])
          Buffer <- (Range_x[2]-Range_x[1])*0.05
          xaxis$limits <- c(Range_x[1]-Buffer, Range_x[2]+Buffer)
          if(XLog)xaxis$limits <- 10^xaxis$limits
        }
        if (is.null(yaxis$limits)){
          Range_y <-range(obj$data[,1])
          #add 4% white space around plots (like R default in plot)
          Buffer <- (Range_y[2]-Range_y[1])*0.04
          yaxis$limits <- c(Range_y[1]-Buffer, Range_y[2]+Buffer)
          if(YLog) yaxis$limits <- 10^yaxis$limits
        }

        #Make plot
        nicePlot(xaxis,yaxis,log=log,xlab=xlab, ylab=ylab,
                 frame.plot = frame.plot, tck=tck,...)
      }
      else
        plot(X,Y, type='n', log=log, xlab=xlab, ylab=ylab,
             frame.plot = frame.plot, tck=tck,...)
    }

    #add datapoints	--------------------------------
    if(type %in% c("o","b", "p")){
      if(obj$gt == "none")
        points(X, Y, col = col[1], pch=pch[1],...)
      else{
        for(i in 1:ngrps){
          iref  <- as.character(obj$data[,3]) == groups[i]
          points(X[iref], Y[iref], col =col[i], pch=pch[i],...)
        }
      }
    }

    #add lines --------------------------------
    if(type %in% c("o","b", "l")){

      #decide which coefficients to use: alternative (default) or null
      if(use.null==FALSE)
        coef <- obj$coef
      else
        coef <- obj$nullcoef

      #determine end points for lines
      if(is.null(from[1])){  #based on fitted values
        for(i in 1:ngrps){
          from[i] <- as.numeric(obj$from[i])
          to[i] <- as.numeric(obj$to[i])
        }
      } else {  #user defined
        if(length(from) == 1){
          from <- rep(from[1], ngrps)
          to <- rep(to[1], ngrps)
        }
      }

      #add lines to plot
      for(i in 1:ngrps){
        #coefficients
        a <- coef[[i]][1,1]
        B <-  coef[[i]][2,1]
        r2 <- obj$groupsummary$r2[i]
        #     browser()
        p <- obj$groupsummary$p[i]

        col.tr <- make.transparent(col[i], max(0, (1 - p/line.transp.p)))
        #        browser()
        #choose line according to log-trsnaformation used in fitting data, even if different transformation used for axes
        if(log_data=="xy")
          curve(10^a*x^B, from[i], to[i], add=TRUE,col = col.tr, lty= lty[i],...)
        if(log_data=="x")
          curve(a+B*log10(x), from[i], to[i], add=TRUE, col = col.tr, lty= lty[i],...)
        if(log_data=="y")
          curve(exp(a+x*B), from[i], to[i], add=TRUE, col = col.tr, lty= lty[i],...)
        if(log_data=="")
          curve(a + x*B, from[i], to[i], add=TRUE,  col = col.tr, lty= lty[i],...)
      }
    }
  }

  # RESIDUAL PLOT
  if(whichplot == "residual")
  {
    #obtain data--------------------------------
    Y <- fitted.sma(obj, type = "residuals")
    X <- fitted.sma(obj, type = "fitted")


    #axis labels--------------------------------
    #determine axis labels if not already specified
    if(is.null(xlab)) xlab <- paste("Fitted values (",names(obj$data)[2], " v ",
                                    names(obj$data)[1],")")
    if(is.null(ylab)) ylab <- paste("Residual values (",names(obj$data)[2], " v ",
                                    names(obj$data)[1],")")

    #SETUP AXES--------------------------------
    if(!add){ #use default plotting options
      plot(X,Y, type='n', xlab=xlab, ylab=ylab, frame.plot = frame.plot,...)
    }

    #add datapoints	--------------------------------
    if(type %in% c("o","b", "p")){
      if(obj$gt == "none")
        points(X, Y, col = col[1], pch=pch[1],...)
      else{
        for(i in 1:ngrps){
          iref <- as.character(obj$data[,3]) == groups[i]
          points(X[iref], Y[iref], col =col[i], pch=pch[i],...)
        }
      }
    }
  }

  # QQ PLOT
  if(whichplot == "qq")
  {
    #obtain data--------------------------------
    Y <- fitted.sma(obj, type = "residuals")

    #axis labels--------------------------------
    #determine axis labels if not already specified
    if(is.null(xlab)) xlab <- "Normal quantiles"
    if(is.null(ylab)) ylab <- paste("Residual values (",names(obj$data)[2], " v ",
                                    names(obj$data)[1],")")

    #SETUP AXES--------------------------------
    if(add==FALSE){ #use default plotting options
      qqnorm(Y, xlab=xlab, ylab=ylab,...)
      qqline(Y)
    }
  }

}
