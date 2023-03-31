# Part 1: Linear Regression to Predict MPG
library(dplyr)
setwd("C:\\Users\\esegu\\Desktop\\CHALLENGE PENDIENTE\\Module 16\\Starter_Code (17)")
# Import and read in the csv file as a data frame
mecha_mpg <- read.csv('MechaCar_mpg.csv', check.names = FALSE)

# Linear regression using lm() function for all 5 variables.
mecha_car_1 <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_mpg)

# Summary() to determine p-value and the r-squared value.
summary(mecha_car_1)

# Part 2: Visualization for the Trip Analysis
# Import csv
suspension_coil <- read.csv('Suspension_Coil.csv', check.names = FALSE)

summary <- suspension_coil %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# Part 3 - T-Test on Suspension Coils
t.test(suspension_coil$PSI, mu = 1500)

# Write 3 more scripts using t.test() and its subset() argument to determine if the PSI for 
# each manufacturing lot is statistically different from the
# population mean of 1500 pounds per square inch.

# For Lot1
t.test(subset(suspension_coil, Manufacturing_Lot == 'Lot1')$PSI, mu = 1500)

# For Lot2
t.test(subset(suspension_coil, Manufacturing_Lot == 'Lot2')$PSI, mu = 1500)

# For Lot3
t.test(subset(suspension_coil, Manufacturing_Lot == 'Lot3')$PSI, mu = 1500)

