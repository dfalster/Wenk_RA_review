
#SET PARAMETERS
params <- function(){
  p <- list()
  #canopy shape parameters from Yokozawa et al 1995
  p$eta <- 12
  
  #ratio leaf area to sapwood area
  p$theta <- 4669
  p$theta <- 10000
  
  #height  - leaf area scaling
  p$a1 <- 5.44
  p$B1 <- 0.306
  
  #leaf area -stem volume scaling
  p$a2 <- 6.67E-5
  p$B2 <- 1.75
  
  #root leaf scaling
  p$a3 <- 0.07
  
  #scaling of leaf turnover(/yr) to LMA
  p$a4 <- 2.86E-2*0.5  #TROPICAL RATe
  p$B4 <- 1.71
  
  p$b <- 0.17
  
  #nitrogen concentrations & photosynthesis
  p$n_area <- 1.87E-3    #leaf kg/m2
  p$c_p1 <- 150.36
  p$c_p2 <- 0.19
  
  #respiration rates
  p$c_Rl <- 2.1E4 #mol / kg / yr
  p$c_Rs <- 4012  #mol / m3 / yr
  p$c_Rr <- 217   #mol / kg / yr
  p$c_Rb <- 2*p$c_Rs
  
  #carbon conversion parameter
  p$Y <- 0.7
  p$c_bio <- 12E-3/0.49
  
  #turnover
  p$k_b <- 0.2
  p$k_r <- 1.0
  p$k_s <- 0.2;
  
  #REPRODUCTION
  p$c_r1 <- 0  # set to zero for this analysis
  p$c_r2 <- 50
  p$c_acc <- 4
  p
}

baseTraits <- function(){
  traits <- list()
  traits$lma <- 1.11E-01
  traits$rho <- 608;
  traits$hmat <- 100;
  traits
}

#ALLOMETRIC MODEL FOR SIZE OF CONMPONENTS
Height <- function(A){p$a1*A^p$B1}
dHdA <- function(A){p$B1*p$a1*A^(p$B1-1)}
LeafArea <- function(h){ (h/p$a1)^(1/p$B1)}
LeafMass <- function(lma, A){A*lma}
RootMass <- function(A){p$a3*A}
XSAreaBark <- function(A){p$b*XSAreaSapwood(A)}
XSAreaSapwood <- function(A){A/p$theta}
XSArea <- function(A, XSAh=XSAreaHeartwood(A)){XSAreaBark(A)+XSAreaSapwood(A)+XSAh}
XSAreaHeartwood <- function(A){(p$a2*A^p$B2)/etac(p$eta)/Height(A)}
SapwoodMass <- function(rho, A,h=Height(A)){rho*XSAreaSapwood(A)*etac(p$eta)*h}
etac <- function(eta){1-2/(1+eta)+1/(1+2*eta)}
BarkMass <- function(rho, A,h){p$b*SapwoodMass(A,h, rho)}
HeartwoodMass <- function(rho, A){rho*p$a2*A^p$B2}

LiveMass <- function(traits, A){
  ml <- LeafMass(traits$lma, A)
  ms <- SapwoodMass(traits$rho,A,Height(A))
  mb <- BarkMass(traits$rho,A,Height(A))
  mr <- RootMass(A)
  return(ml+ms+mb+mr)
}

TotalMass <- function(traits, A,  mh= HeartwoodMass(traits$rho,A)){
  return(LiveMass(traits, A)+mh)
  }

#SOLVE HEIGHT FOR GIVEN MASS
Height.mt <- function(traits, mt){
  #returns mass for given height
  LiveMass.wrap <- function(x, traits,mt){LiveMass(traits, LeafArea(x))-mt}
  y <- 0*mt
  for(i in 1:length(mt)){y[i] <- uniroot(LiveMass.wrap, c(0, 50), traits=traits, mt=mt[i])$root}
  return(y)}

#reproductive allocation
ReproductiveAllocation <- function(hmat,h){p$c_r1/(1+exp(p$c_r2*(1-h/hmat)))}

#production functions
dMtdt <- function(traits, h, env){
  Production(traits, h, env)*(1-ReproductiveAllocation(traits$hmat,h)) 
}

Production <- function(traits, h, env=1, A=LeafArea(h), ms = SapwoodMass(traits$rho,A,h), mb=BarkMass(traits$rho,A,h),  mr = RootMass(A)){
  Assim(A, env) - Respiration(A, ms/traits$rho, mb/traits$rho,  mr) - Turnover(traits, A*traits$lma, ms, mb,  mr)
}

Assim <- function(A, env){
  p$Y*p$c_bio*A*p$c_p1 * env/(p$c_p2+env)
}

Respiration <- function(A, vs, vb,  mr){
  Respiration.leaf(A) + Respiration.sapwood(vs) + Respiration.bark(vb) + Respiration.root(mr)
}

Respiration.leaf <- function(A){p$Y*p$c_bio *(p$c_Rl*p$n_area*A)}
Respiration.sapwood <- function(vs){p$Y*p$c_bio * p$c_Rs*vs}
Respiration.bark <- function(vb){p$Y*p$c_bio *2*p$c_Rs*vb}
Respiration.root <- function(mr){p$Y*p$c_bio * p$c_Rr*mr}

Turnover <- function(traits, ml, ms, mb,  mr){
  Turnover.leaf(traits$lma, ml) + Turnover.sapwood(ms) + Turnover.bark(mb) + Turnover.root(mr)
}
Turnover.leaf <- function(LMA, ml){(p$a4*LMA^-p$B4)*ml}
Turnover.sapwood <- function(ms){p$k_s*ms}
Turnover.bark <- function(mb){p$k_b*mb}
Turnover.root <- function(mr){p$k_r*mr}

#HEIGHT GROWTH RATE
dHdt <- function(traits, h, dMtdt, r){
  dHdA(LeafArea(h))*dAdMt(traits, LeafArea(h))*dMtdt*r
}

#MARGINAL COST OF LEAF AREA GROWTH
dMldA <- function(lma,A){A*0+lma}
dMsdA <- function(rho, A){rho/p$theta*etac(p$eta)*(Height(A)+A*dHdA(A))}
dMbdA <- function(rho, A){p$b*dMsdA(rho,A)}
dMrdA <- function(A){A*0+p$a3}
dMtdA <- function(traits,A){dMldA(traits$lma, A) + dMbdA(traits$rho,A) + dMsdA(traits$rho,A) + dMrdA(A)}
dAdMt <- function(traits, A){1/dMtdA(traits, A)}


Figure2 <- function(){

  p <<- params()
  traits <- baseTraits()

  cols <- niceColors(5)
  
  #load library deSolve
  library(deSolve)

  dydt <- function(Time, State, Pars) {

    with(as.list(c(State, Pars)),{
      dM <- dMtdt(traits, H, E)  # mass production
      r <- RAS[["Partial bang"]](H, mat, RAinit, RAmax, A50) # reproductive allocation
      dH <- dHdt(traits, H, dM, 1-r)  # height growth
      dS <- -k*S                      # survivorship
      dR <- (r * dM)*S                # Reproductive output
      return(list(c(dH, dS, dR)))
    })
  }

  # Generate list of parameters to run ode model with
  pars <- list()
  pars[[1]] <- c(E=1, k=0.1, mat=10, RAinit=0.5, RAmax=1, A50=5)
  pars[[2]] <- c(E=1, k=0.1, mat=30, RAinit=0.5, RAmax=1, A50=5)
  pars[[3]] <- c(E=1, k=0.1, mat=2, RAinit=0.5, RAmax=1, A50=5)
  pars[[4]] <- c(E=1, k=0.1, mat=10, RAinit=0.1, RAmax=0.6, A50=5)
  pars[[5]] <- c(E=1, k=0.1, mat=30, RAinit=0.1, RAmax=0.6, A50=5)

  # run ode model
  out <- list()
  for( i in 1: length(pars))
    out[[i]] <- ode(y=c(H=0.2,S=1,R=0), times=seq(0, 70, by = 0.1),
          func=dydt, parms=pars[[i]])

  par(mfrow=c(1,3), mar=c(5,5,1,1), oma=c(2,2,2,2))

  # RAS
  plot(0,0, type='n', xaxs="i", yaxs="i", xlim=c(0, 50), ylim=c(-0,1), xlab = "Plant height (m)", ylab = "Reproductive allocation (0-1)", las=1)

  x = seq(0, 50, by=0.1)
  for( i in 1:length(pars))
    points(x,RAS[["Partial bang"]](x, pars[[i]][["mat"]], pars[[i]][["RAinit"]], pars[[i]][["RAmax"]], pars[[i]][["A50"]]), type='l', lwd=1, col=cols[i])
  label(-0.15, 1.2, "a)",xpd=NA)

  # Height
  plot(0,0, xlim=c(0,70), ylim = c(0, 50), type='n', xaxs="i", yaxs="i", xlab = "Time (yr)", ylab = "Height (m)", las=1)
  for( i in 1: length(out))
    points(out[[i]][,1],out[[i]][,1+1], type='l', col=cols[i])
  label(-0.15, 1.2, "b)",xpd=NA)

  # Lifetime seed production
  plot(0,0, xlim=c(0,70), ylim = c(1, 275), type='n',  xaxs="i", yaxs="i", xlab = "Time (yr)", ylab = "Total reproductive output (kg)", log="", las=1)
  for( i in 1: length(out))
    points(out[[i]][,1],out[[i]][,3+1], type='l', col=cols[i])
  label(-0.15, 1.2, "c)",xpd=NA)

}
