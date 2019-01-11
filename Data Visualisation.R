setwd("C:/Users/pc/Documents/R Project")

library(RCurl)
library(XML)
library(ggplot2)
library(dplyr)

###Reading Data from HTML Page and preparing Data frame of required Table

htmlContent<-getURL("file:///C:/Users/pc/Desktop/MostValuesStrtup.html",
                    ssl.verifypeer = FALSE) #ensure that the html content would get copied into the R object. 

head(htmlContent)

##Here we parse the content of HTML Page to identify blocks with similar pattern
htmlParsed<-htmlTreeParse(htmlContent,
                          useInternal = TRUE)
head(htmlParsed)

?unlist
?xpathApply

##Now we read all the Tables present in the Parsed Data & store it as a list format
abc<-readHTMLTable(htmlParsed)
abc$the_list

##We convert the list we are interested in to a Data Frame
def<-as.data.frame(abc$the_list)

###Data Preparation, Cleaning (Getting Data Ready for Visualisation)

str(def) #Here all the columns are factor we need to convert them to char or num accordingly
summary(def)

##Dropping unwanted column from data frame by column index number

def<-def[,-c(1)] 

##Removing Rows with NA in it
def<-na.omit(def)

##Removing  $ sign from def data frame
def$`Brand Value`<-gsub('[$]','',def$`Brand Value`)

def$`Brand Revenue`<-gsub('[$]','',def$`Brand Revenue`)

def$`Company Advertising`<-gsub('[$]','',def$`Company Advertising`)

##To remove "#" from rank variable

?gsub
def$Rank<-gsub('#','',def$Rank)

##Dealing with values '-' in some columns
def$`Company Advertising`[1:20]

def$`Company Advertising`<-gsub('-','',def$`Company Advertising`) 
#In the above step we change our values '-' with NULL, now when we use as.numeric
#the column data type will change from factor/character to numeric and will also change NULL to NA

##Converting Millions into Billions
?ifelse()
?which
?match
?grepl

g<-which(grepl(" M",def$`Brand Value`))
g  #Brand Value column has no values in millions

h<-which(grepl(" M",def$`Brand Revenue`))
h  #Brand Revenue has no values in millions

i<-which(grepl(" M",def$`Company Advertising`))
i  #Only Company Advertising has some values in millions

def$`Company Advertising`<-gsub('B|M','',def$`Company Advertising`)

class(def$`Company Advertising`)

def$`Company Advertising`<-as.numeric(def$`Company Advertising`)

class(def$`Company Advertising`)

def$`Company Advertising`[i]

def$`Company Advertising`[i] <- def$`Company Advertising`[i]/1000

def$`Company Advertising`
class(def$`Company Advertising`)

##Remove "M", "B" from the Data Frame and convert the columns into their respective data types

#Brand Value
def$`Brand Value`<-gsub('B|M','',def$`Brand Value`)

class(def$`Brand Value`)

def$`Brand Value`<-as.numeric(def$`Brand Value`)

class(def$`Brand Value`)

#Brand Revenue
def$`Brand Revenue`<-gsub('B|M','',def$`Brand Revenue`)

class(def$`Brand Revenue`)

def$`Brand Revenue`<-as.numeric(def$`Brand Revenue`)

class(def$`Brand Revenue`)

#Rank
def$Rank<-as.integer(def$Rank)
class(def$Rank)

#1-Yr Value Change
def$`1-Yr Value Change`<- gsub("[%]","",def$`1-Yr Value Change`)

def$`1-Yr Value Change`<- as.integer(def$`1-Yr Value Change`)

class(def$`1-Yr Value Change`)

#Brand
def$Brand<-as.character(def$Brand)

class(def$Brand)

#Technology
def$Industry<-as.character(def$Industry)

class(def$Industry)

str(def)

summary(def)

###remove "#" from rank variable

?gsub
def$Rank<-gsub('#','',def$Rank)

i<-which(grepl(" M",def$`Company Advertising`))
i

def$`Company Advertising`<-gsub('B','',def$`Company Advertising`)

def$`Company Advertising`<-gsub('M','',def$`Company Advertising`)

class(def$`Company Advertising`)

def$`Company Advertising`<-as.numeric(def$`Company Advertising`)

class(def$`Company Advertising`)

def$`Company Advertising`[i]

def$`Company Advertising`[i] <- def$`Company Advertising`[i]/1000

def$`Company Advertising`
class(def$`Company Advertising`)

ghi<-def %>% filter(Industry=="Technology" & `Company Advertising`!='NA')

plot<-ggplot(ghi,aes(x=`Company Advertising`,y=`Brand Revenue`,colour=Brand,size=`Brand Value`))
q<- plot+geom_point()

q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $") #continue from here for Ques4

#Use geom_text , with aes label. You can play with hjust, vjust to adjust text position.
#geom_text(aes(label=Name),hjust=0, vjust=0)

q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+
  geom_text(aes(label=Brand),hjust=0.5,vjust=1)

#guides(color=FALSE) will turn off legends for Colour = Brand i.e., no Brand legend at right side
#theme_light : light gray lines and axis (more attention towards the data)

q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+
  geom_text(aes(label=Brand),hjust=0.5,vjust=1)+guides(color=FALSE)+theme_light()

#Use theme() to modify individual components of a theme, 
#allowing you to control the appearance of all non-data components of the plot. 

#legend.key to control background underneath legend keys (element_rect; inherits from rect)

?theme

q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+
  geom_text(aes(label=Brand),hjust=0.5,vjust=1)+guides(color=FALSE)+theme_light()+
  theme(legend.key=element_rect(fill="light blue", color="black"))+ggtitle("Technology")

#scale_size(name = waiver(), breaks = waiver(), labels = waiver(),
#limits = NULL, range = c(1, 6), trans = "identity", guide = "legend")

?scale_shape_discrete()

plot<-ggplot(ghi,aes(x=`Company Advertising`,y=`Brand Revenue`,colour=Brand,size=`Brand Value`))
q<- plot+geom_point()

q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+
  scale_size(range = c(5,10),breaks = c(40,80,120,160),name = "Brand Value$(Billions)")+
  geom_text(aes(label=Brand),hjust=0.5,vjust=1)+guides(color=FALSE)+theme_light()+
  theme(legend.key=element_rect(fill="light blue", color="black"))+ggtitle("Technology")

##Technology

data1<- data %>% filter(Industry=="Technology")

p<-ggplot(data1,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,colour=Brand))
q<- p+geom_point()

q+xlab("Company Advertising in $")+
  ylab("Brand Revenue $") +
  scale_size(range = c(5,10),breaks = c(30,60,90),name = "Brand Value $ (Billions)")+
  theme_light()+
  ggtitle("Technology")+
  scale_color_discrete(name="p$Brand")

###Industry = Financial Services

data1<- data %>% filter(Industry=="Financial Services")

p<-ggplot(data1,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,colour=Brand))
q<- p+geom_point()

q+xlab("Company Advertising in $")+
  ylab("Brand Revenue $") +
  scale_size(range = c(5,10),breaks = c(30,60,90),name = "Brand Value $ (Billions)")+
  theme_light()+
  ggtitle("Financial Services")+
  scale_color_discrete(name="p$Brand")
