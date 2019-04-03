#####Load data after selecting only lunch meal sales fpr elementary schools from original POS data
school.lunch <- read.csv("/Users/Hanh/Desktop/Farm to School Program/Data/New Salad2.csv", header = T)
unique(school.lunch$Description)
unique(school.lunch$Site)
unique(school.lunch$Date)

#Creating a dummy based on Description with salad
school.lunch$dummy=grepl('Salad',school.lunch$Description)

#Seperate data to only salad
salad=subset(school.lunch,school.lunch$dummy==1) 

#Summation of all salad entrees for each school based on each day
attach(salad)
library(plyr)
salad=with(salad,ddply(salad,c("Site","Date","dummy"),summarise,sum=sum(SubTotal)))
write.csv(salad, file="salad.csv")

#Seperate data to only non salad 
non.salad=subset(school.lunch,school.lunch$dummy==0) 
#Summation of all nonsalad entrees for each school based on each day
library(plyr)
attach(non.salad)
non.salad <- with(non.salad,ddply(non.salad,c("Site","Date","dummy"),summarise,sum=sum(SubTotal)))
write.csv(non.salad, file="non.salad.csv")
colnames(non.salad) <- c("Site", "Date", "sum")

#Descriptive Stats for salad
library(Hmisc)
describe(non.salad$sum)
boxplot.stats(non.salad$sum)
summary(non.salad$sum)
fivenum(non.salad$sum)
library(pastecs)
stat.desc(non.salad$sum) 
library(psych)
describe(non.salad$sum)


#### Selecting each school based on salad and non-salad sales

#Alachua
Alachua.nonsalad <- subset(non.salad, Site=="Alachua ")
Alachua.salad <- subset(salad, Site=="Alachua ")
write.csv(Alachua.nonsalad, file="Alachua.nonsalad.csv")
write.csv(Alachua.salad, file="Alachua.salad.csv")

#Archer.Community 
Archer.nonsalad <- subset(non.salad, Site=="Archer.Community")
Archer.salad <- subset(salad, Site=="Archer.Community")
write.csv(Archer.nonsalad, file="Archer.nonsalad.csv")
write.csv(Archer.salad, file="Archer.salad.csv")

#Chiles.Lawton 
Chiles.nonsalad <- subset(non.salad, Site=="Chiles.Lawton")
Chiles.salad <- subset(salad, Site=="Chiles.Lawton")
write.csv(Chiles.nonsalad, file="Chiles.nonsalad.csv")
write.csv(Chiles.salad, file="Chiles.salad.csv")

#Finley total.nonsalad for each day
Finley.nonsalad <- subset(non.salad, Site=="Finley")
Finley.salad <- subset(salad, Site=="Finley")
write.csv(Finley.nonsalad, file="Finley.nonsalad.csv")
write.csv(Finley.salad, file="Finley.salad.csv")

#Foster.Stephen total.nonsalad for each day
Foster.nonsalad <- subset(non.salad, Site=="Foster.Stephen")
Foster.salad <- subset(salad, Site=="Foster.Stephen")
write.csv(Foster.nonsalad, file="Foster.nonsalad.csv")
write.csv(Foster.salad, file="Foster.salad.csv")

#Glen.Springs total.nonsalad for each day
Glen.nonsalad <- subset(non.salad, Site=="Glen.Springs")
Glen.salad <- subset(salad, Site=="Glen.Springs")
write.csv(Glen.nonsalad, file="Glen.nonsalad.csv")
write.csv(Glen.salad, file="Glen.salad.csv")

#Hidden.Oak total.nonsalad for each day
Hidden.nonsalad <- subset(non.salad, Site=="Hidden.Oak")
Hidden.salad <- subset(salad, Site=="Hidden.Oak")
write.csv(Hidden.nonsalad, file="Hidden.nonsalad.csv")
write.csv(Hidden.salad, file="Hidden.salad.csv")

#High.Springs.Community.School total.nonsalad for each day
High.Springs.nonsalad <- subset(non.salad, Site=="High.Springs.Community.School")
High.Springs.salad <- subset(salad, Site=="High.Springs.Community.School")
write.csv(High.Springs.nonsalad, file="High.Springs.nonsalad.csv")
write.csv(High.Springs.salad, file="High.Springs.salad.csv")

#Idylwild total.nonsalad for each day
Idylwild.nonsalad <- subset(non.salad, Site=="Idylwild ")
Idylwild.salad <- subset(salad, Site=="Idylwild ")
write.csv(Idylwild.nonsalad, file="Idylwild.nonsalad.csv")
write.csv(Idylwild.salad, file="Idylwild.salad.csv")

#Irby.W.W. total.nonsalad for each day
Irby.nonsalad <- subset(non.salad, Site=="Irby.W.W.")
Irby.salad <- subset(salad, Site=="Irby.W.W.")
write.csv(Irby.nonsalad, file="Irby.nonsalad.csv")
write.csv(Irby.salad, file="Irby.salad.csv")

#Lake.Forest total.nonsalad for each day
Lake.Forest.nonsalad <- subset(non.salad, Site=="Lake.Forest ")
Lake.Forest.salad <- subset(salad, Site=="Lake.Forest ")
write.csv(Lake.Forest.nonsalad, file="Lake.Forest.nonsalad.csv")
write.csv(Lake.Forest.salad, file="Lake.Forest.salad.csv")

#Littlewood total.nonsalad for each day
Littlewood.nonsalad <- subset(non.salad, Site=="Littlewood ")
Littlewood.salad <- subset(salad, Site=="Littlewood ")
write.csv(Littlewood.nonsalad, file="Littlewood.nonsalad.csv")
write.csv(Littlewood.salad, file="Littlewood.salad.csv")

#Meadowbrook total.nonsalad for each day
Meadowbrook.nonsalad <- subset(non.salad, Site=="Meadowbrook")
Meadowbrook.salad <- subset(salad, Site=="Meadowbrook")
write.csv(Meadowbrook.nonsalad, file="Meadowbrook.nonsalad.csv")
write.csv(Meadowbrook.salad, file="Meadowbrook.salad.csv")

#Metcalfe total.nonsalad for each day
Metcalfe.nonsalad <- subset(non.salad, Site=="Metcalfe")
Metcalfe.salad <- subset(salad, Site=="Metcalfe")
write.csv(Metcalfe.nonsalad, file="Metcalfe.nonsalad.csv")
write.csv(Metcalfe.salad, file="Metcalfe.salad.csv")

#Newberry total.nonsalad for each day
Newberry.nonsalad <- subset(non.salad, Site=="Newberry")
Newberry.salad <- subset(salad, Site=="Newberry")
write.csv(Newberry.nonsalad, file="Newberry.nonsalad.csv")
write.csv(Newberry.salad, file="Newberry.salad.csv")

#Norton.C.W total.nonsalad for each day
Norton.nonsalad <- subset(non.salad, Site=="Norton.C.W")
Norton.salad <- subset(salad, Site=="Norton.C.W")
write.csv(Norton.nonsalad, file="Norton.nonsalad.csv")
write.csv(Norton.salad, file="Norton.salad.csv")

#Rawlings total.nonsalad for each day
Rawlings.nonsalad <- subset(non.salad, Site=="Rawlings")
Rawlings.salad <- subset(salad, Site=="Rawlings")
write.csv(Rawlings.nonsalad, file="Rawlings.nonsalad.csv")
write.csv(Rawlings.salad, file="Rawlings.salad.csv")

#Shell total.nonsalad for each day
Shell.nonsalad <- subset(non.salad, Site=="Shell")
Shell.salad <- subset(salad, Site=="Shell")
write.csv(Shell.nonsalad, file="Shell.nonsalad.csv")
write.csv(Shell.salad, file="Shell.salad.csv")

#Talbot.Wm total.nonsalad for each day
Talbot.nonsalad <- subset(non.salad, Site=="Talbot.Wm")
Talbot.salad <- subset(salad, Site=="Talbot.Wm")
write.csv(Talbot.nonsalad, file="Talbolt.nonsalad.csv")
write.csv(Talbot.salad, file="Talbolt.salad.csv")

#Terwilliger total.nonsalad for each day
Terwilliger.nonsalad <- subset(non.salad, Site=="Terwilliger")
Terwilliger.salad <- subset(salad, Site=="Terwilliger")
write.csv(Terwilliger.nonsalad, file="Terwilliger.nonsalad.csv")
write.csv(Terwilliger.salad, file="Terwilliger.salad.csv")

#Wiles.Kimball total.nonsalad for each day
Wiles.nonsalad <- subset(non.salad, Site=="Wiles.Kimball")
Wiles.salad <- subset(salad, Site=="Wiles.Kimball")
write.csv(Wiles.nonsalad, file="Wiles.nonsalad.csv")
write.csv(Wiles.salad, file="Wiles.salad.csv")

#Williams total.nonsalad for each day
Williams.nonsalad <- subset(non.salad, Site=="Williams")
Williams.salad <- subset(salad, Site=="Williams")
write.csv(Williams.nonsalad, file="Williams.nonsalad .csv")
write.csv(Williams.salad, file="Williams.salad.salad.csv")


#####Data for each school were exported to excel and daily salad & non-salad meals sales were matched for each school by dates
#####For each school observations with just a salad or non-salad meale for a day is deleted
#####Product.Source variable was added to the data in excel
#####Codes below merge population data with POS data after finding salad ratio

FTS <- read.csv("/Users/Hanh/Desktop/Farm to School Program/Book2.csv", header = T)
demographic <- read.csv("/Users/Hanh/Desktop/Desktop Data/demographics.csv", header = T)
FTS <- read.csv("/Users/Hanh/Desktop/Farm to School Program/Data.csv", header = T)
#Delete NA rows
FTS <- subset(FTS,!(is.na(FTS["non.salad"]) | is.na(FTS["salad"])))

##Calculating salad ratio
FTS$salad.ratio <- FTS$salad/(FTS$salad + FTS$non.salad)
FTS$salad.ratio=round(FTS$salad.ratio, digits=4)
FTS$total.meal <- FTS$salad + FTS$non.salad
format(round(FTS$salad.ratio, 2))

##Attaching demograhics data
merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
    
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}

data <- merge.with.order(FTS, demographic, by='site', all.y = T, sort=F, keep_order = 1)

##Renaming product source
data$product.source <- sub("^$", "Other", data$product.source)
data$product.source <- sub("Other", "1", data$product.source)
data$product.source <- sub("FTS", "2", data$product.source)
data$product.source <- sub("Florida", "3", data$product.source)


##Export data to CSV file for further editing
write.csv(data, file="Data.csv")
write.csv(FTS, file="Data1.csv")
######Data exported to excel and taste profile variables were attached to the data
