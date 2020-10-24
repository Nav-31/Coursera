# Coursera

# Application of Machine Learning
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.

The goal of this project is to quantify exercises performed by six participants analyzing the data obtained from accelerometers of dumbell, forearm,arm and belt


```{r,warning=FALSE,message=FALSE}
library(caret)
library(rpart)
library(ggplot2)
library(corrplot)
set.seed(1000)
```
### Loading Dataset:
```{r,warning=FALSE,message=FALSE}
tstUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trUrl  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

tst <- read.csv(url(tstUrl))[,-1]
tr <- read.csv(url(trUrl))[,-1]
```


### Data Visualization:
```{r,warning=FALSE,message=FALSE}
dim(tr)
dim(tst)
```
### Data Exploration for Preprocessing the Data:
```{r,warning=FALSE,message=FALSE}
#Cleaning the Data:
NV <- nearZeroVar(tr)
tr <- tr[, -NV]
tst <- tst[, -NV]

NV <- sapply(tr, function(x) mean(is.na(x))) > 0.9
tr <- tr[, NV == "FALSE"]
tst <- tst[, NV == "FALSE"]

# Clearing Time and ID values
tr<- tr[,-c(1:5)]
tst <- tst[,-c(1:5)]

dim(tr)
dim(tst)
table(tr$classe)
```
### Modelling with Cross Validation:
```{r,warning=FALSE,message=FALSE}
set.seed(1234)
cv3 = trainControl(method="cv",number=3,allowParallel=TRUE,verboseIter=TRUE)
modrf = train(classe~., data=tr, method="rf",trControl=cv3)
modtree = train(classe~.,data=tr,method="rpart",trControl=cv3)
```


```{r}
prf=predict(modrf,tr)
ptree=predict(modtree,tr)
table(prf,tr$classe)
```


```{r}
table(ptree,tr$classe)
```

```{r,warning=FALSE,message=FALSE}
prf=predict(modrf,tst)
ptree=predict(modtree,tst)
table(prf,ptree)
```

### Conclusion:
```{r,warning=FALSE,message=FALSE}
r=predict(modrf,tst)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
r
```
The Predicted Classes for the TestCases are as follows:
```{r,warning=FALSE,message=FALSE}
pml_write_files(r)
```

