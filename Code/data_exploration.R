# Load the data
source("03_load_data.R")

library(skimr)
library(ggplot2)
library(DataExplorer)
library(plyr)
library(dplyr)
library(sqldf)
library(reshape2)
library(gridExtra)
library(skimr)

skimr::skim(adult)

DataExplorer::plot_bar(adult, nrow = 1)

DataExplorer::plot_histogram(adult, nrow = 1)

DataExplorer::plot_boxplot(adult, by = "income", ncol = 3)

ggplot(adult) + aes(x=as.numeric(age), group=income, fill=income) +
  geom_histogram(binwidth=1, color='black')

  # histogram of age by gender group
ggplot(adult) + aes(x=as.numeric(age), group=sex, fill=sex) +
    geom_histogram(binwidth=1, color='black')

# summarize the class distribution
percentage <- prop.table(table(adult$income)) * 100
cbind(freq=table(adult$income), percentage=percentage)

#work class
df  <- adult
df$income<-ifelse(df$income=='>50K',1,0)
df$workclass<-ifelse(df$workclass=='?','Unknown',as.character(df$workclass))
Work_class<-sqldf('SELECT workclass, count(workclass) as Count
                  ,sum(income) as Above from df group by workclass')
table<-data.frame(Class=Work_class$workclass, Proportion=Work_class$Above/Work_class$Count)
Work_class$Below<-Work_class$Count-Work_class$Above
Work_class<-Work_class[,c(1,3,4)]
Workclass<-melt(Work_class,id.vars = 'workclass')
gg<-ggplot(Workclass,aes(x=workclass,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different classes')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))

#education
education<-sqldf('SELECT education, count(education) as Count
                 ,sum(income) as Above from df group by education')
education$Below<-education$Count-education$Above
table<-data.frame(Class=education$education, Proportion=education$Above/education$Count)
education<-education[,c(1,3,4)]
edu<-melt(education,id.vars = 'education')
gg<-ggplot(edu,aes(x=education,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different education level')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
            nrow=2,
            as.table=TRUE,
            heights=c(1,4))

#Marital

colnames(df)[6]<-'Marital'
marital<-sqldf('SELECT Marital, count(Marital) as Count
                  ,sum(income) as Above from df group by Marital')
marital$Below<-marital$Count-marital$Above
table<-data.frame(Marital=marital$Marital, Proportion=marital$Above/marital$Count)
marital<-marital[,c(1,3,4)]
mar<-melt(marital,id.vars = 'Marital')
gg<-ggplot(mar,aes(x=Marital,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different marital status')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))
