### Question 2 ###

#load packages
install.packages("multcomp")
install.packages("car")
library(car)
library(multcomp)

install.packages("tidyverse")
install.packages("ggpubr")
library(tidyverse)
library(ggpubr)

#Statistical things to consider:
#Explanatory Variable: WT vs KO (categorical)
#Response Variables: Systolic Blood Pressure and Heart Rate
#Nuisance Variables: Gestational Day, may affect readings, but need to account for it
#Covariates: Baselines For each mouse

bloodpressureSummary <- data.frame()

for (x in 1:12) {
  bloodpressureSummary[x,1] = mean(Jcad_rawData[x,2],Jcad_rawData[x,5],Jcad_rawData[x,8],Jcad_rawData[x,11])
  bloodpressureSummary[x,2] = Jcad_rawData[x,14]
  bloodpressureSummary[x,3] = Jcad_rawData[x,17]
  bloodpressureSummary[x,4] = Jcad_rawData[x,20]
  bloodpressureSummary[x,5] = Jcad_rawData[x,23]
  bloodpressureSummary[x,6] = Jcad_rawData[x,26]
  bloodpressureSummary[x,7] = Jcad_rawData[x,29]
  bloodpressureSummary[x,8] = Jcad_rawData[x,32]
  bloodpressureSummary[x,9] = Jcad_rawData[x,35]
}
bloodpressureSummary <- cbind(Jcad_rawData$Genotype,bloodpressureSummary)
names(bloodpressureSummary)<-c('Genotype',
                          'baseline_systolicBP',
                          'E4.5_systolicBP',
                          'E6.5_systolicBP',
                          'E8.5_systolicBP',
                          'E10.5_systolicBP',
                          'E12.5_systolicBP',
                          'E14.5_systolicBP',
                          'E16.5_stytolicBP',
                          'E18.5_systolicBP')

heartrateSummary <- data.frame()

for (x in 1:12) {
  heartrateSummary[x,1] = mean(Jcad_rawData[x,3],Jcad_rawData[x,6],Jcad_rawData[x,9],Jcad_rawData[x,12])
  heartrateSummary[x,2] = Jcad_rawData[x,15]
  heartrateSummary[x,3] = Jcad_rawData[x,18]
  heartrateSummary[x,4] = Jcad_rawData[x,21]
  heartrateSummary[x,5] = Jcad_rawData[x,24]
  heartrateSummary[x,6] = Jcad_rawData[x,27]
  heartrateSummary[x,7] = Jcad_rawData[x,30]
  heartrateSummary[x,8] = Jcad_rawData[x,33]
  heartrateSummary[x,9] = Jcad_rawData[x,36]
}
heartrateSummary <- cbind(Jcad_rawData$Genotype,heartrateSummary)
names(heartrateSummary)<-c('Genotype',
                           'baseline_HR',
                           'E4.5_HR',
                           'E6.5_HR',
                           'E8.5_HR',
                           'E10.5_HR',
                           'E12.5_HR',
                           'E14.5_HR',
                           'E16.5_HR',
                           'E18.5_HR')


#Part one: create a timecourse dataframe to visualise the data

timecourse_df <- data.frame()
for (x in 1:9){
  timecourse_df[x,1] = (x*4.5)-((x-1)*2.5)
  timecourse_df[x,2] = mean(bloodpressureSummary[bloodpressureSummary$Genotype=='WT',x+1],na.rm = TRUE)
  timecourse_df[x,3] = mean(bloodpressureSummary[bloodpressureSummary$Genotype=='KO',x+1],na.rm = TRUE)
  timecourse_df[x,4] = mean(heartrateSummary[heartrateSummary$Genotype=='WT',x+1],na.rm = TRUE)
  timecourse_df[x,5] = mean(heartrateSummary[heartrateSummary$Genotype=='KO',x+1],na.rm = TRUE)
}
names(timecourse_df)<-c('Time',
                    'WT_BP',
                    'KO_BP',
                    'WT_HR',
                    'KO_HR')

p1<-ggplot(timecourse_df,aes(Time)) + 
  geom_line(aes(y = WT_BP, color = 'WT')) + 
  geom_line(aes(y = KO_BP, color = 'ΔJcad')) +
  xlab('time / days')+
  ylab('mean systolic blood pressure / mmHg')+
  ggtitle('Time course of systolic blood pressure during gestation')

p2<-ggplot(timecourse_df, aes(Time)) + 
  geom_line(aes(y = WT_HR, colour = 'WT')) + 
  geom_line(aes(y = KO_HR, colour = 'ΔJcad')) +
  xlab('time / days')+
  ylab('mean heart rate / bp')+
  ggtitle('Time course of heart rate during gestation')

ggarrange(p1,p2)

#Here it appears that there is no obvious linear relationship between
#gestation and HR or BP. However the parameters appear to vary wildly
#during gestation

# PART 2: ANOVA

#In light of our observations we will use a 2-way ANOVA, blocking for
#gestation day in order to account for these fluctuations

#Create a dataset with columns 'Genotype','Time','WT_BP','KO_BP','WT_HR','KO_HR'

pregnancyDF = data.frame()
row = 1
for (y in 1:12){
  for (x in 1:8){
    pregnancyDF[row,1]= bloodpressureSummary[y,1] #genotype
    pregnancyDF[row,2]= as.character((x*4.5)-((x-1)*2.5)) #time
    pregnancyDF[row,3]= bloodpressureSummary[y,x+2] #BP
    pregnancyDF[row,4]= heartrateSummary[y,x+2] #HR
    row = row + 1
  } 
}
names(pregnancyDF)<-c('Genotype',
                      'Time',
                      'bloodPressure',
                      'heartRate')

#convert time into a factor

pregnancyDF$Genotype <- as.factor(pregnancyDF$Genotype)
pregnancyDF$Time <- as.factor(pregnancyDF$Time)

#fist we must check the normality of the data using a Shapiro Wilk test

shapiro.test(pregnancyDF$bloodPressure)#normal
shapiro.test(pregnancyDF$heartRate)#normal (less so)

#next we will carry out a Levene test to test the homskedity of variance

leveneTest(pregnancyDF$bloodPressure ~ pregnancyDF$Genotype, data = pregnancyDF) #Can conduct test
leveneTest(pregnancyDF$heartRate ~ pregnancyDF$Genotype, data = pregnancyDF) #can conduct test

# Now plug into an ANOVA

fit1 <- aov(pregnancyDF$bloodPressure~pregnancyDF$Genotype+pregnancyDF$Time,data=pregnancyDF)
summary(fit1)
fit2 <- aov(pregnancyDF$heartRate~pregnancyDF$Genotype+pregnancyDF$Time,data=pregnancyDF)
summary(fit2)

#Boxplots

p3 <- ggplot(pregnancyDF, aes(x=Genotype, y=bloodPressure, fill=Genotype))+
  geom_boxplot()+
  xlab('Genetic Background')+
  ylab('Systolic Blood Pressure (mmHg)')+
  ggtitle('Blood pressure during preganancy in WT and ΔJcad (n=6)')+
  theme_bw()

p4 <- ggplot(pregnancyDF, aes(x=Genotype, y=heartRate, fill=Genotype))+
  geom_boxplot()+
  xlab('Genetic Background')+
  ylab('Heart Rate (bpm)')+
  ggtitle('Heart rate during pregnancy in WT and ΔJcad (n=6)')+
  theme_bw()

ggarrange(p3,p4)

# PART 3 : Post-hoc test

TukeyHSD(fit1)
TukeyHSD(fit2)

