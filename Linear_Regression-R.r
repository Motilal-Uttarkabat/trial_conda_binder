#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(plotly)                                        # Loading library for linear regression and interactive plots 
library(ggplot2)                                       # Loading library for quick and easy plots

data <- read.csv(args)                                 # Reading the CSV file from iput argument from command line
cat('Reading the file', args)

png("r_orig.png")                                      # Output to a png file
fig1 <- ggplot(data, aes(x = x, y = y)) + geom_point() # Scatter plot from data, aes-axes, geom_point-marker
print(fig1)                                            # Printing the plot to the output png file
dev.off()
print('Printing a scatter plot')

# Adding linear fit
png("r_lm.png")                                        # Output to a png file
fig2 <- fig1 + geom_point(color = "red") + geom_smooth(method = "lm", formula ="y ~ x", se = FALSE)               
print(fig2)                                            # Printing the plot to the output png file
dev.off()
print('Printing a scatter plot with linear fit')
