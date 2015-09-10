library(smatr)
source("R/plot-utils.R")

# Figure 1 - example RA schedules
source("R/RAS-functions.R")
to.pdf(plotRASexamples(),"output/Fig1.pdf", height=6, width=12)

# Figure 2 - plot consequences of RAS for size and seed output
source("R/growth-functions.R")
to.pdf(Figure2(),"output/Fig2.pdf", height=3, width=9)

# Figure 3 - plot of Reproductive output vs leaf area using data from Henery & Westoby 2001
source("R/RO-data.R")
to.pdf(plot_henery_data(),"output/Fig3.pdf", height=6, width=6)
