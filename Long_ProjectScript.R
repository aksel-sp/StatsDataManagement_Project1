# import libraries
library(multcomp)
library(tidyverse)
library(car)
library(ggpubr)
# load raw data
Jcad_rawData <- read.table("/Users/aksel/OneDrive/Documents/DPhil Work/MT 2020 Taught Work/Statistics and Data Management/StatsDataManagement_Project1/Blood_Pressure_Data_project_2.txt",skip = 1, fill= TRUE,header = TRUE)
Jcad_rawData <- Jcad_rawData[-c(1:3)]
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
#create a useable dataframe
gestationState_df = data.frame()
row = 1
for (y in 1:12){
  for (x in 1:12){
    gestationState_df[row,1]= Jcad_rawData[y,1] #genotype
    if (is.element(x, c(1,2,3,4))== TRUE){
      gestationState_df[row,2]= FALSE
      gestationState_df[row,3]= as.character(0) #time
      gestationState_df[row,4]= NA #baseline BP
      gestationState_df[row,5]= NA #baseline HR
      gestationState_df[row,6]= Jcad_rawData[y,(x*3)-1] #baseline BP
      gestationState_df[row,7]= Jcad_rawData[y,x*3] #baseline HR
      }else{
        gestationState_df[row,2]= TRUE
        gestationState_df[row,3]= as.character((x*4.5)-((x-1)*2.5)-6) #time
        gestationState_df[row,4]= mean(Jcad_rawData[y,2],Jcad_rawData[y,5],Jcad_rawData[y,8],Jcad_rawData[y,11]) #baseline BP
        gestationState_df[row,5]= mean(Jcad_rawData[y,3],Jcad_rawData[y,6],Jcad_rawData[y,9],Jcad_rawData[y,12]) #baseline HR
        gestationState_df[row,6]= Jcad_rawData[y,(x*3)-1] #BP
        gestationState_df[row,7]= Jcad_rawData[y,x*3] #HR
      }
    row = row + 1
  } 
}
names(gestationState_df)<-c('Genotype',
                           'pregnancyState',
                           'gestationDay',
                           'baseline_bloodPressure',
                           'baseline_heartRate',
                           'bloodPressure',
                           'heartRate')

gestationState_df$Genotype <- factor(gestationState_df$Genotype)
gestationState_df$gestationDay <- factor(gestationState_df$gestationDay)
gestationState_df$pregnancyState <- factor(gestationState_df$pregnancyState)

scatter<-ggplot(data=gestationState_df, aes(x=bloodPressure,y=heartRate,colour=Genotype))+
  geom_point()+
  xlab('Systolic Blood Pressure (mmHg)')+
  ylab('Heart Rate (bpm)')+
  ggtitle('Systolic Blood Pressure vs Heart Rate')+
  theme_bw()

hist1<-ggplot(data=gestationState_df, aes(x=bloodPressure))+
  geom_histogram(fill='red',colour='black')+
  xlab('Systolic Blood Pressure (mmHg)')+
  ylab('Frequency')+
  theme_bw()
hist2<-ggplot(data=gestationState_df, aes(x=heartRate))+
  geom_histogram(fill='blue',colour='black')+
  xlab('Heart Rate (bpm)')+
  ylab('Frequency')+
  theme_bw()
hists<-ggarrange(hist1,hist2)

ggarrange(scatter,hists, nrow=2 , ncol=1)

# Part 2: Checking assumptions

shapiro.test(gestationState_df$bloodPressure)#normal
shapiro.test(gestationState_df$heartRate)#normal (less so)

#next we will carry out a Levene test to test the homskedity of variance

leveneTest(gestationState_df$bloodPressure ~ gestationState_df$Genotype, data = gestationState_df) #Can conduct test
leveneTest(gestationState_df$heartRate ~ gestationState_df$Genotype, data = gestationState_df) #can conduct test

# Question 1 : Does genetic background affect the HR or BP in the baseline

Q1BP_ANOVA <- aov(bloodPressure ~ Genotype,gestationState_df[gestationState_df$'pregnancyState'=='FALSE',])
summary(Q1BP_ANOVA)
Q1BP_postHoc <- glht(Q1BP_ANOVA,linfct = mcp(Genotype='Tukey'))
summary(Q1BP_postHoc)

Q1HR_ANOVA <- aov(heartRate ~ Genotype,gestationState_df[gestationState_df$'pregnancyState'=='FALSE',])
summary(Q1HR_ANOVA)
Q1HR_postHoc <- glht(Q1HR_ANOVA,linfct = mcp(Genotype='Tukey'))
summary(Q1HR_postHoc)

# Question 2 : Does Genetic background affect the HR or BP during Pregnancy

# Here I will conduct an ANCOVA using the baseline as a covariate and blocking
# for gestationday

Q2BP_ANCOVA <- aov(bloodPressure ~ Genotype+baseline_bloodPressure+gestationDay,gestationState_df)
summary(Q2BP_ANCOVA)
Q2BP_postHoc <- glht(Q2BP_ANCOVA,linfct = mcp(Genotype='Tukey'))
summary(Q2BP_postHoc)

Q2HR_ANCOVA <- aov(heartRate ~ Genotype+baseline_heartRate+gestationDay,gestationState_df)
summary(Q2HR_ANCOVA)
Q2HR_postHoc <- glht(Q2HR_ANCOVA,linfct = mcp(Genotype='Tukey'))
summary(Q2HR_postHoc)

# Question 3 : Does pregnancy influence systolic blood pressure and heart rate?

Q3BP_ANCOVA <- aov(bloodPressure ~ pregnancyState+Genotype,gestationState_df)
summary(Q3BP_ANCOVA)
Q3BP_postHoc <- glht(Q3BP_ANCOVA,linfct = mcp(pregnancyState='Tukey'))
summary(Q3BP_postHoc)

Q3HR_ANCOVA <- aov(heartRate ~ pregnancyState+Genotype,gestationState_df)
summary(Q3HR_ANCOVA)
Q3HR_postHoc <- glht(Q3HR_ANCOVA,linfct = mcp(pregnancyState='Tukey'))
summary(Q3HR_postHoc)

# Question 4: Are there any interactions between pregnancy state, 
# gestation day and genetic background with respect to systolic 
# blood pressure? 

Q4BP_ANCOVA <- aov(bloodPressure~pregnancyState*gestationDay*Genotype,gestationState_df)
summary(Q4BP_ANCOVA)
Q4BP_postHoc <- glht(Q4BP_ANCOVA,linfct = mcp(pregnancyState='Tukey'))
summary(Q4BP_postHoc)

Q4HR_ANCOVA <- aov(heartRate~pregnancyState*gestationDay*Genotype,gestationState_df)
summary(Q4HR_ANCOVA)
Q4HR_postHoc <- glht(Q4HR_ANCOVA,linfct = mcp(pregnancyState='Tukey'))
summary(Q4HR_postHoc)


