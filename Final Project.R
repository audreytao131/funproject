#Import Data
ChildHealthData <-read.table("C:/Users/RabiaS/Desktop/2011_2012_NSCH.txt", header = TRUE,
                 sep = ",")

#Remove Columns that are not needed for anaylis by listing the names of the varibales
#and selecting for the ones the are needed 

names(ChildHealthData)
ChildHealthData<-ChildHealthData[c(3,6,378,379,396,616,617,618,619,620,621,622,623,624,626)]

#Rename the columns 
colnames(ChildHealthData) <- c("State","Age","Sex","Race","OverallHealth","ACEincome",
                               "ACEdivorce","ACEdeath","ACEjail","ACEdomviol",
                               "ACEneighviol","ACEmhealth","ACEdrug","ACEdiscrim","ACEct")
statetable<-table(ChildHealthData$State)
statetable
prop.table((statetable))

agetable<-table(ChildHealthData$Age)
agetable
prop.table(agetable)

agecattable<-table(ChildHealthData$Agecat1)
agecattable
prop.table((agecattable))

sextable<-table(ChildHealthData$Sex)
sextable
prop.table(sextable)

racetable<-table(ChildHealthData$Race)
racetable
prop.table(racetable)

healthtable<-table(ChildHealthData$OverallHealth)
healthtable
prop.table(healthtable)

aceincometable<-table(ChildHealthData$ACEincome)
aceincometable
prop.table(aceincometable)


acedivorcetable<-table(ChildHealthData$ACEdivorce)
acedivorcetable
prop.table(acedivorcetable)

acedeathtable<-table(ChildHealthData$ACEdeath)
acedeathtable
prop.table(acedeathtable)

acejailtable<-table(ChildHealthData$ACEjail)
acejailtable
prop.table(acejailtable)

acedomvioltable<-table(ChildHealthData$ACEdomviol)
acedomvioltable
prop.table(acedomvioltable)

aceneighvioltable<-table(ChildHealthData$ACEneighviol)
aceneighvioltable
prop.table(aceneighvioltable)

acemhealthtable<-table(ChildHealthData$ACEmhealth)
acemhealthtable
prop.table(acemhealthtable)

acedrugtable<-table(ChildHealthData$ACEdrug)
acedrugtable
prop.table(acedrugtable)

acediscrimtable<-table(ChildHealthData$ACEdiscrim)
acediscrimtable
prop.table(acediscrimtable)

acecttable<-table(ChildHealthData$ACEct)
acecttable
prop.table(acecttable)

ChildHealthData$ACEcat1<-cut(ChildHealthData$ACEct, c(1,2,9))
ChildHealthData$ACEcat1

l                                 
                                    
#Set the missing values
ChildHealthData$Sex[ChildHealthData$Sex==6]<- NA
ChildHealthData$Sex[ChildHealthData$Sex==7]<- NA
ChildHealthData$OverallHealth[ChildHealthData$OverallHealth==99]<- NA
ChildHealthData$Race[ChildHealthData$Race==999]<- NA
ChildHealthData$Race[ChildHealthData$Race==99]<- NA
ChildHealthData$ACEincome[ChildHealthData$ACEincome==(999)] <- NA
ChildHealthData$ACEincome[ChildHealthData$ACEincome==(99)] <- NA
ChildHealthData$ACEdivorce[ChildHealthData$ACEdivorce==999] <- NA
ChildHealthData$ACEdivorce[ChildHealthData$ACEdivorce==99] <- NA
ChildHealthData$ACEdeath[ChildHealthData$ACEdeath==999] <- NA
ChildHealthData$ACEdeath[ChildHealthData$ACEdeath==99] <- NA
ChildHealthData$ACEjail[ChildHealthData$ACEjail==999] <- NA
ChildHealthData$ACEjail[ChildHealthData$ACEjail==99] <- NA
ChildHealthData$ACEdomviol[ChildHealthData$ACEdomviol==999] <- NA
ChildHealthData$ACEdomviol[ChildHealthData$ACEdomviol==99] <- NA
ChildHealthData$ACEneighviol[ChildHealthData$ACEneighviol==999] <- NA
ChildHealthData$ACEneighviol[ChildHealthData$ACEneighviol==99] <- NA
ChildHealthData$ACEmhealth[ChildHealthData$ACEmhealth==999] <- NA
ChildHealthData$ACEmhealth[ChildHealthData$ACEmhealth==99] <- NA
ChildHealthData$ACEdrug[ChildHealthData$ACEdrug==999] <- NA
ChildHealthData$ACEdrug[ChildHealthData$ACEdrug==99] <- NA
ChildHealthData$ACEdiscrim[ChildHealthData$ACEdiscrim==999] <- NA
ChildHealthData$ACEdiscrim[ChildHealthData$ACEdiscrim==99] <- NA
ChildHealthData$ACEct[ChildHealthData$ACEct==99] <- NA

#Find the missing values
sum(is.na(ChildHealthData$Sex))
sum(is.na(ChildHealthData$Race))
sum(is.na(ChildHealthData$OverallHealth))
sum(is.na(ChildHealthData$ACEincome))
sum(is.na(ChildHealthData$ACEdivorce))
sum(is.na(ChildHealthData$ACEdeath))
sum(is.na(ChildHealthData$ACEjail))
sum(is.na(ChildHealthData$ACEdomviol))
sum(is.na(ChildHealthData$ACEneighviol))
sum(is.na(ChildHealthData$ACEmhealth))
sum(is.na(ChildHealthData$ACEdiscrim))
sum(is.na(ChildHealthData$ACEct))

#create a table to find the proportion of missing values in each variable
table1<-sapply(ChildHealthData, function(x) sum(is.na(x)))
table2<-prop.table(table1)
table2

#looking at missing values
install.packages("naniar")
library(naniar)
vis_miss(ChildHealthData, warn_large_data = FALSE)

#Correlations and Frequencies 
ChildHealthData$Agecat1<-cut(ChildHealthData$Age, c(0,5,12,17))
ChildHealthData$Agecat1
table3<-table(ChildHealthData$Agecat1,ChildHealthData$Sex,useNA = "always")
table3
prop.table(table3)
summary(table3)

table4<-table(ChildHealthData$Agecat1,ChildHealthData$Race,useNA = "always")
table4
prop.table(table4)
summary(table4)

table5<-table(ChildHealthData$Agecat1,ChildHealthData$ACEincome,useNA = "always")
table5
prop.table(table5)
summary(table5)

table6<-table(ChildHealthData$Agecat1,ChildHealthData$ACEdivorce,useNA = "always")
table6
prop.table(table6)
summary(table6)

table7<-table(ChildHealthData$Agecat1,ChildHealthData$ACEdeath,useNA = "always")
table7
prop.table(table7)
summary(table7)

table8<-table(ChildHealthData$Agecat1,ChildHealthData$ACEjail,useNA = "always")
table8
prop.table(table8)
summary(table8)

table8<-table(ChildHealthData$Agecat1,ChildHealthData$ACEjail,useNA = "always")
table8
prop.table(table8)
summary(table8)

table10<-table(ChildHealthData$Agecat1,ChildHealthData$ACEneighviol,useNA = "always")
table8
prop.table(table8)
summary(table8)

table11<-table(ChildHealthData$Agecat1,ChildHealthData$ACEmhealth,useNA = "always")
table11
prop.table(table11)
summary(table11)

table12<-table(ChildHealthData$Agecat1,ChildHealthData$ACEdrug,useNA = "always")
table12
prop.table(table12)
summary(table12)

table13<-table(ChildHealthData$Agecat1,ChildHealthData$ACEdiscrim,useNA = "always")
table13
prop.table(table13)
summary(table13)

table14<-table(ChildHealthData$Agecat1,ChildHealthData$ACEct,useNA = "always")
table14
prop.table(table14)
summary(table14)


cor(ChildHealthData$OverallHealth,ChildHealthData$ACEct,use="complete.obs")
hist(ChildHealthData$ACEct)
hist(ChildHealthData$Age)
hist(ChildHealthData$Race)
