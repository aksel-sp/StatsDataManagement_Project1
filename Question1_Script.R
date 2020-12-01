### Question 1 ###

#load packages
install.packages("tidyverse")
install.packages("ggpubr")
library(tidyverse)
library(ggpubr)

### PART 1 : LOADING AND CLEANING DATA
#load data: need to account for a double header and remove the identification columns (not necessary for analysis)

Jcad_rawData <- read.table("/Users/aksel/OneDrive/Documents/DPhil Work/MT 2020 Taught Work/Statistics and Data Management/StatsDataManagement_Project1/Blood_Pressure_Data_project_2.txt",skip = 1, fill= TRUE,header = TRUE)
Jcad_rawData <- Jcad_rawData[-c(1:3)]

# Now we will need to rename the columns using dplr

names(Jcad_rawData) <- c("Genotype",
                         "B1_systolicBP","B1_pulse","B1_validTraces",
                         "B2_systolicBP","B2_pulse","B2_validTraces",
                         "B3_systolicBP","B3_pulse","B3_validTraces",
                         "B4_systolicBP","B4_pulse","B4_validTraces",
                         "E4.5_systolicBP","E4.5_pulse","E4.5_validTraces",
                         "E6.5_systolicBP","E6.5_pulse","E6.5_validTraces",
                         "E8.5_systolicBP","E8.5_pulse","E8.5_B1_validTraces",
                         "E10.5_systolicBP","E10.5_pulse","E10.5_validTraces",
                         "E12.5_systolicBP","E12.5_pulse","E12.5_validTraces",
                         "E14.5_systolicBP","E14.5_pulse","E14.5_validTraces",
                         "E16.5_systolicBP","E16.5_pulse","E16.5_validTraces",
                         "E18.5_systolicBP","E18.5_pulse","E18.5_validTraces")

# Jcad_rawdata is now a dataframe with good names and all the values need for statistical analysis

### PART 2: Does the genetic background (WT vs KO) of the mice influence systolic blood pressure and heart rate ? ###

#Statistical things to consider:
#Explanatory Variable: WT vs KO (categorical)
#Response Variables: Systolic Blood Pressure and Heart Rate
#Nuisance Variables: N/A
#Covariates: N/A

# Create summary data frame 

baselineSummary <- data.frame(baseline_meanBP = c(),
                              baseline_meanPulse = c())

for (x in 1:12) {
  baselineSummary[x,1] = mean(Jcad_rawData[x,2],Jcad_rawData[x,5],Jcad_rawData[x,8],Jcad_rawData[x,11])
  baselineSummary[x,2] = mean(Jcad_rawData[x,3],Jcad_rawData[x,6],Jcad_rawData[x,9],Jcad_rawData[x,12])
}
baselineSummary <- cbind(Jcad_rawData$Genotype,baselineSummary)
names(baselineSummary)<-c('Genotype','baseline_meanBP','baseline_meanHR')

#First - Blood Pressure

p1 <- ggplot(baselineSummary, aes(x=Genotype, y=baseline_meanBP, fill=Genotype))+
  geom_boxplot()+
  xlab('Genetic Background')+
  ylab('Systolic Blood Pressure (mmHg)')+
  ggtitle('Mean baseline blood pressure in WT and ΔJcad (n=6)')+
  theme_bw()

t.test(baselineSummary$baseline_meanBP~baselineSummary$Genotype)

#Second - Heart Rate

p2 <- ggplot(baselineSummary, aes(x=Genotype, y=baseline_meanHR, fill=Genotype))+
  geom_boxplot()+
  xlab('Genetic Background')+
  ylab('Heart Rate (bpm)')+
  ggtitle('Mean baseline heart rate in WT and ΔJcad (n=6)')+
  theme_bw()

t.test(baselineSummary$baseline_meanHR~baselineSummary$Genotype)

ggarrange(p1,p2)



