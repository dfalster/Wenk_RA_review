

RAS <- list()

RAS[["Big bang"]] <- function(x, mat=0.5){
	y <- x*0 + 1
	y[x < mat] <- 0
	y
}

RAS[["Partial bang"]] <- function(x, mat=0.5, RAinit=0.5, RAmax=0.7, A50=0.05*mat){
	y <- RAinit + michaelis_menton(x-mat, A50=A50)*(RAmax - RAinit)
	y[x < mat] <- 0
	y
}

RAS[["Asymptotic"]] <- function(x, ...){
	 RAS[["Partial bang"]](x, RAinit=0,...)
}


RAS[["Gradual - indeterminate"]] <- function(x, mat=0.5, A50=2*mat,  ...){
	RAS[["Partial bang"]](x, RAinit=0, mat=mat, A50=A50, ...)
}

RAS[["Gradual - determinate"]] <- function(x, mat=0.5, A50=0.5,  ...){
	RAS[["Partial bang"]](x, RAinit=0, RAmax=2, mat=mat, A50=A50, ...)
}

# based on formula for a parabola (x-h)^2 = 4p(y-k), with vertex (h,k)
RAS[["Declining"]] <- function(x, mat=0.5, h=0.65, k=0.6, p=-0.1){
	y <- k + (x^2 - 2*h*x +h^2)/(4*p)
	y[x < mat] <- 0
	y
}

rectagular_hyperbolae <- function(X,  Amax, QY, theta, phi){
	(phi*X + Amax -  pow((phi*X + Amax)^2 - 4*theta*X*Amax*phi, 0.5))/ (2 * theta)
}

michaelis_menton <- function(x, A50, Amax=1){
	Amax * x / (x+A50)
}


plotRASexamples <- function(){

	par(oma = c(2,2,2,2))
	layout(matrix(c(1,2,3,4,1, 5,6,7), 2, 4, byrow=TRUE), c(2,1,1,1), c(1,1))
	#layout.show(7) ## show the regions that have been allocated to each plot

	new_plot <- function(){
		blank_plot(xlim=c(0, 1), ylim=c(-0,1))
		axis(2, at = c(0,1), las = 1)
	}

  col <- niceColors(3)[3]
	x<-seq(0,1, by=0.001)
	new_plot()
	mtext("Plant size",1, 3)
	mtext("Reproductive allocation (0-1)", 2, 3)

	title("a) Elements of a reproductive allocation schedule")

	mat = 0.25
	RAinit = 0.4
	RAmax = 0.8
	points(x, RAS[["Partial bang"]](x, mat=mat,  RAinit=RAinit, A50=0.05,  RAmax=RAmax), type='l', lwd=2)

	axis(1, at = c(mat), las = 0, labels = c("Size at maturation"), col=col, col.axis = col)

	points(c(0,mat), c(RAinit, RAinit), lty = "dashed", col= col, type='l')
	text(0.5*mat, 1.1*RAinit, labels = c("RA at maturation"), col=col)

	points(c(0,1), c(RAmax, RAmax), lty = "dashed", col= col, type='l')
	text(0.5*mat, 1.1*RAmax, labels = c("Maximum RA"), col=col)

	for(i in 1:length(names(RAS))) {
		n = names(RAS)[i]

		new_plot()
		if(n == "Big bang")
			y <- RAS[[n]](x, mat=0.5)
		if(n == "Partial bang")
			y <- RAS[[n]](x, mat=0.5,  RAinit=0.6, A50=0.05,  RAmax=0.8)
		if(n == "Asymptotic")
			y <- RAS[[n]](x, mat=0.5, A50=0.05,  RAmax=0.4)
		if(n == "Gradual - indeterminate")
			y <- RAS[[n]](x, mat=0.5)
		if(n == "Gradual - determinate")
			y <- RAS[[n]](x, mat=0.5)
		if(n == "Declining")
			y <- RAS[[n]](x, mat=0.5)

		points(x, y, type='l', lwd=2, col=col)
		title(paste(letters[i+1], ") ", n))
		if(i==5)
			mtext("Plant size",1, 3)
	}
}


plot_RAS_growth_seedouput <- function(){

	par(oma = c(2,2,2,2))
	#layout(matrix(c(1,2,3,4,), 1, 4, byrow=TRUE))

	cols =niceColors()

	ras <-list()
	ras[[1]] <- function(x) RAS[["Big bang"]](x, mat=1)
	ras[[2]] <- function(x) RAS[["Big bang"]](x, mat=10)
	ras[[3]] <- function(x) RAS[["Big bang"]](x, mat=20)
	ras[[4]] <- function(x) RAS[["Partial bang"]](x, mat=1)
	ras[[5]] <- function(x) RAS[["Partial bang"]](x, mat=10)
	ras[[6]] <- function(x) RAS[["Partial bang"]](x, mat=20)

	h <- seq(0,40, by=0.001)

	# firs plot: RAS
	blank_plot(xlim=c(0, 40), ylim=c(0,1))
	axis(2, at = c(0,1), las = 1)
	mtext("Plant height (m)",1, 3)
	mtext("Reproductive allocation (0-1)", 2, 3)
	for(i in 1:length(ras)) {
		points(h, ras[[i]](h), type='l', lwd=2, col= cols[i])

	}
}
