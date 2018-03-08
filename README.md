---
title: "MWRDAnalysis"
author: "UIC"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,cache = TRUE,autodep = TRUE)
```



The analysis of the given datasets which are as following is done on R.


##### 1. Service Desk
##### 2. Outage Notification
##### 3. Change Request Form

We have included the code within the documentation within the document to let the technical users leverage it.First we will be analyzing the Service Desk dataset. 



```{r echo=TRUE }

library(readxl)
ServiceDeskModified <- read_excel("D:/UIC/3rd Semester/Capstone/datafrommwrd/ServiceDeskModified.xlsx")
Sdm<-ServiceDeskModified



```

Function to know which column has categorical values so that it can be converted to factor variables.Have to use nrow , length does not work for a tibble

```{r echo=TRUE }
differentValues<-data.frame()
for(i in 1:ncol(Sdm)){
  differentValues[i,1]<-nrow(unique(Sdm[,i]))
}
colsFactor<-Sdm[!seq(from = 1,to = ncol(Sdm),by = 1) %in% which(differentValues>20)]
Sdm[,c(2:4,6,7,8)]<-lapply(colsFactor,FUN =factor)
```


Adding few columns after change in requirements

```{r message=FALSE,warning=FALSE}
library(readxl)
ServiceDesk_Data <- read_excel("D:/UIC/MyResearch/Mwrd/mwrdAnalysis/ServiceDesk Data.xlsx", 
    col_types = c("numeric", "numeric", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "date", "date", "text", 
        "text", "numeric", "numeric", "numeric", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text"))
ServiceDesk_Data<-ServiceDesk_Data[,c(4,11,12,18)]
Sdm<-cbind(Sdm,ServiceDesk_Data)

```



Let's take a look at the dataset and the variables after a bit of modification


```{r echo=FALSE,results="asis"}
library(knitr)
kable(Sdm[1:5,],caption = "Service Desk ")
```

## Exploratory Analysis

How many <span style="color:red">tickets</span> have been resolved


```{r echo=FALSE,message=FALSE}
library(ggplot2)
g<-ggplot(data=Sdm,aes(STATE))+
geom_bar()
plot(g)
```

From the plot we see that majority of them are closed ? We don't have the date embedded
in the data source file else we would have calculated 

What <span style="color:red">category</span> are most of my tickets ?


```{r echo=FALSE}
library(ggplot2)
g1<-ggplot(data=Sdm,aes(CATEGORY))+
geom_bar(aes(fill=CATEGORY))+
theme(axis.text.x = element_blank())
plot(g1)
```

From the plot we see that majority of them are closed ? We don't have the date embedded
in the data source file else we would have calculated 


Which <span style="color:red">Plant</span> are raising the most requests for tickets ?

```{r echo=FALSE}
library(ggplot2)
g2<-ggplot(data=Sdm,aes(SITE))+
geom_bar(aes(fill=SITE))+
theme(axis.text.x = element_blank())
plot(g2)
```

From the plot we see that majority of them are closed ? We don't have the date embedded
in the data source file else we would have calculated 

Which <span style="color:red">department</span> is raising most of the tickets ?


```{r echo=FALSE}
library(ggplot2)
g3<-ggplot(data=Sdm,aes(DEPARTMENT))+
geom_bar(aes(fill=DEPARTMENT))+
theme(axis.text.x = element_blank())

plot(g3)
```


From the plot we see that majority of them are closed 

We want to see who has created <span style="color:red">most of the tickets</span>

```{r results="asis"}
library(knitr)
Sdm$CREATED_BY<-as.factor(Sdm$CREATED_BY)
Most_Tickets_Created<-as.data.frame(head(sort(table(Sdm$CREATED_BY),decreasing = T),n=50))
kable(Most_Tickets_Created,caption = "Most_Tickets_Created")

```




###Going a bit deeper into Exploratory analysis 

Since our concerned departments are <span style="color:red">MOB</span> and <span style="color:red">MOBA</span> we are filtering data and then analyzing by department

```{r message=FALSE}
library(dplyr)
g4<-ggplot(data=(Sdm %>% filter(SITE %in% c("MOBA","MOB"))),aes(x=SITE,Y=DEPARTMENT))+
geom_bar(aes(x=SITE,fill=DEPARTMENT))

plot(g4)
```

**We see that most of the tickets raised are for Information Technology department in MOB and Engineering department in MOBA**

Let's us see the priority of tickets in these two departments

```{r message=FALSE}
library(dplyr)
g4<-ggplot(data=(Sdm %>% filter(SITE %in% c("MOBA","MOB"))),aes(x=SITE,Y=PRIORITY))+
geom_bar(aes(x=SITE,fill=PRIORITY))

plot(g4)
```

**In both the departments tickets generated are of priority medium**

# Text Mining on the Description column 

The approach is to to count the frequency of words that matter and remove those words out which are of common English and to remove those words which have zero information content ( from our perspective and client's perspective )

<span style="color:red">Step 1 :</span> Collapse all the descriptions into a paragraph
<span style="color:red">Step 2 :</span> Replace punctuations such as ,_:/ etc.
<span style="color:red">Step 3 :</span> Repace all the digits
<span style="color:red">Step 3 :</span> Replace single letter words
<span style="color:red">Step 4 :</span> Replace stopwords words such as a,and,of,the
<span style="color:red">Step 5 :</span> Delete additional words in a retrospective manner 
<span style="color:red">Step 6 :</span> Make a textbag of words and after that create a frequency table 



```{r warning=FALSE,message=FALSE}
##### Text Mining
text<-paste(Sdm$DESCRIPTION_NO_HTML,collapse = "")
write(text,"description.txt")
#punctuation replacement
text2<-gsub(pattern = "\\W",replacement = " ",text)
# digits replacement
text2<-gsub(pattern="\\d",replacement = "",text2)
text2<-gsub(pattern = "\\b[A-z]\\b{1}",replacement = "",text2)

library(tm)
# words removed with no information
text2<-removeWords(text2,stopwords())
library(stringr)
textbagWithoutCleaning<-str_split(text2,pattern="\\s+")
textbagWithoutCleaning<-unlist(textbagWithoutCleaning)
write.csv(head(sort(table(textbagWithoutCleaning),decreasing = T),n=300),file = "withoutcleaning.csv")
text2<-removeWords(text2,words=c("mwrd","Please","org","User","can","will","The","Thanks","user","From","Thank","To","reached","get","MWRD","see","th","like","This","Can","able","If","We","For","know","use","It","Hi","need","needed","needs","please","Help","My","now","trying","used","help","also","via"))
textbag<-str_split(text2,pattern="\\s+")
textbag<-unlist(textbag)
write.csv(head(sort(table(textbag),decreasing = T),n=300),file = "withcleaning.csv")

```



We will be creating a wordcloud as well as the frequencies of the word


**Creating a wordcloud**


```{r warning=FALSE,message=FALSE}
library(RColorBrewer)
library(wordcloud)
wordcloud(textbag,min.freq = 250,random.order = FALSE,colors =brewer.pal(8,"Dark2"),scale = c(4,0.5))

```




### Correlations 



#Change Request Form

```{r echo=TRUE,message=FALSE,warning=FALSE}
library(readr)
Change_Request_Form<- read_csv("D:/UIC/3rd Semester/Capstone/datafrommwrd/Change Request Form Modified.csv")
```

###A Look at the Dataset

```{r echo=TRUE,message=FALSE,warning=FALSE,results="asis"}
kable(Change_Request_Form[1:5,],caption = "Change Request Form")
```



We see that our data frame has dates.But while importing from a CSV it is read as a character.To convert this column for meaningful purposes we will convert it regex in R.Now we will extract the time and add a column for time

```{r,message=FALSE,warning=FALSE}
library("dplyr")
Change_Request_Form<-mutate(.data = Change_Request_Form,CreatedTime=gsub(pattern ="[0-9]{1,}/[0-9]{1,}/[0-9]{1,}" ,replacement ="" ,x = Change_Request_Form$`Create Date`))

```

We will pull the dates now.

```{r message=FALSE}
library("dplyr")
Change_Request_Form<-mutate(.data = Change_Request_Form,CreatedDate=gsub(pattern ="([0-9]{1,}/[0-9]{1,}/[0-9]{1,}).*" ,replacement ="\\1" ,x = Change_Request_Form$`Create Date`))

```

We need to delete the existing column for Date time and create proper formatting for Date and Time

```{r warning=FALSE,message=FALSE}
Change_Request_Form$CreatedDate<-as.Date(x = Change_Request_Form$CreatedDate,format = "%m/%d/%Y")
Change_Request_Form$CreatedTime<-trimws(Change_Request_Form$CreatedTime,which="left")
library("lubridate")
Change_Request_Form$CreatedTime<-hm(Change_Request_Form$CreatedTime)
Change_Request_Form$`Maintenance Date`<-as.Date(x = Change_Request_Form$`Maintenance Date`,format = "%m/%d/%Y")
Change_Request_Form<-Change_Request_Form[,c(1,8,9,3,4,5,6,7)]


```


**We will be creating a difference in dates which is the most important parameter. This will explain us in how many days the response team responded**
```{r}
Change_Request_Form<-mutate(.data = Change_Request_Form,DiffDays= difftime(time1 = Change_Request_Form$`Maintenance Date`,time2 = Change_Request_Form$CreatedDate,units = "days"))
```

We can get the descriptive statistics for <span style="red">DiffDays</span>

```{r}
summary(as.numeric(Change_Request_Form$DiffDays))
DiffDays<-as.data.frame(as.numeric(Change_Request_Form$DiffDays))


```

We can see that Min=-5 and Max=262.The mean is 16 days and Median is 9 days.
We would like to know how many tickets are solved within a given time duration.
We will be creating bins for different tickets 

* Bins
  + less than 3-days
  + less than 10-days but greater than 3
  + 10 or more days
  
  
```{r results="asis"}
D<-data.frame(IntervalDays=c("(-7)-0","0-7","7-14","14-292"),MaintainFreq=c("13","92","59","70"))
View(D)
g5<-ggplot(data=D)+
geom_col(aes(x=IntervalDays,y=MaintainFreq))
plot(g5)
kable(D[1:4,],caption = "Maintainenance")

```


We see that most of the tickets are resolved in more than 11 days.The ideal time should be decided by the organization but we have decided a time of 0-3 days as ideal time.
We have observed that some tickets have been resolved even before the Create Date 

```{r}
g6<-ggplot(data = Change_Request_Form)+
geom_bar(aes(x=Change_Request_Form$`Responsible IT Area`,fill=Change_Request_Form$`Purpose Group`))+
theme(axis.text.x =element_text(angle = 45, vjust = 0.5 , hjust = 0.5))  

plot(g6)
```

# Analysis of Outage Notification Form

```{r message=FALSE}
library(readr)
Onm<- read_csv("D:/UIC/MyResearch/Mwrd/mwrdAnalysis/Outage Notification Modified.csv", 
    col_types = cols(`Create Date` = col_datetime(format = "%m/%d/%Y %H:%M"), 
        `Outage End Date` = col_date(format = "%m/%d/%Y"), 
        `Outage Start Date` = col_date(format = "%m/%d/%Y")))


```

A look at the dataset

```{r results="asis"}
kable(Onm[1:5,],caption = "Outage Notification Form")
```

How <span style="color:red">long</span> it takes to close the ticket ?


```{r results="asis"}
library("knitr")
library("dplyr")
Onm<-mutate(.data = Onm,OnmDiffDays=difftime(time1 = Onm$`Outage End Date`,time2 = Onm$`Outage Start Date`,units = "days"))
summary(as.numeric(Onm$OnmDiffDays))
setNames(as.data.frame(table(as.numeric(Onm$OnmDiffDays))),c("Outage Lasted","Frequency"))
```


Which<span style="color:red"> IT area </span> the tickets belong to ?

```{r}

g7<-ggplot(data = Onm)+
geom_bar(aes(Onm$`IT Area`,fill=Onm$`IT Area`))+
theme(axis.text.x = element_blank())
plot(g7)
  


```







  Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
