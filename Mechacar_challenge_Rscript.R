#read csv mpg data
mechaCar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# mechar_table dataFrame
as.data.frame(mechaCar_table)

#import dplyr library
library(dplyr)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechaCar_table)

# summary function to determine p-value & r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechaCar_table)) 

#read csv suspension coil
suspension_coil_table <- read.csv(file='Suspension_coil.csv',check.names=F,stringsAsFactors = F)

# summary function to create a total summary data frame (mean, median, variance, and standard deviation of the suspension coil’s PSI column)
total_summary <- suspension_coil_table %>% summarize(Mean_PSI=mean(PSI),
                                           Median_PSI=median(PSI),
                                           Var_PSI=var(PSI),
                                           Std_Dev_PSI=sd(PSI),
                                           Num_Coil=n(), .groups = 'keep') 

# group by function to create a "lot summary"
lot_summary <- suspension_coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                                         Median_PSI=median(PSI),
                                                                         Var_PSI=var(PSI),
                                                                         Std_Dev_PSI=sd(PSI),
                                                                         Num_Coil=n(), .groups = 'keep')


# t-test function to determine PSI in all lots
t.test(suspension_coil_table$PSI,mu=1500)

# individual t-test for each lot to determine if PSI for each lot differs
lot1 <- subset(suspension_coil_table, Manufacturing_Lot=="Lot1")

lot2 <- subset(suspension_coil_table, Manufacturing_Lot=="Lot2")

lot3 <- subset(suspension_coil_table, Manufacturing_Lot=="Lot3")


t.test(lot1$PSI,mu=1500)

t.test(lot2$PSI,mu=1500)

t.test(lot3$PSI,mu=1500)

