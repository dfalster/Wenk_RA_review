
plot_henery_data <- function(){
  par(oma = c(2,2,2,2))
  #load individual data
  Raw <- read.table("data/Henery01-base.txt", header=TRUE, sep="\t", fill=TRUE)
  Species_data <- read.table("data/Henery01-SpeciesMean.txt", header=TRUE, sep="\t", fill=TRUE)

  # match seed mass and accessory data to each indiv

  Raw$Seed_size=Species_data$Seed.mass..mg[match(Raw$Species, Species_data$X)]*10E-6
  Raw$Acc_cost=Species_data$Accessory.cost.g..per.seed[match(Raw$Species, Species_data$X)]*1E-3
  Raw$RO=(Raw$Seed_size + Raw$Acc_cost)*Raw$Seeds

  # standard
  sm1 <- sma(RO ~ Leaf_area_est*Species, data=Raw, log='xy')

  blank_plot(xlim=c(1E-4, 1E2), ylim=c(1E-6, 1E1), log='xy', xaxs="i", yaxs="i")
  axis.log10(side=1, at=-6:2)
  axis.log10(side=2, at=-6:4)
  mtext(expression(paste("Leaf area (",m^2, ")")), 1,3)
  mtext(expression(paste("Reproductive output (kg ", yr^-1, ")")), 2,3)

  plot(sm1, pch =16, col=make.transparent(niceColors(), 1), type='p', add= TRUE)
}
