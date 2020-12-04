getwd()
data=read.csv("binray.csv",header=TRUE)
data=mydata
str(data)
#normalization
hist(data$gre)
data$gre=(data$gre-min(data$gre))/(max(data$gre)-min(data$gre))
data$gpa=(data$gpa-min(data$gpa))/(max(data$gpa)-min(data$gpa))
data$rank=(data$rank-min(data$rank))/(max(data$rank)-min(data$rank))
hist(data$gre)
set.seed(222)
ind=sample(2,nrow(data),replace=TRUE,prob=c(.7,.3))
training=data[ind==1,]
testing=data[ind==2,] 
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = 2,
               err.fct = "ce",
               linear.output = FALSE)
plot(n)  
output=compute(n,training[,-1])
head(output$net.result)
head(training[1,]) 
in4 <- 0.0455 + (0.82344*0.7586206897) + (1.35186*0.8103448276) + (-0.87435*0.6666666667)
in4
out4 <- 1/(1+exp(-in4))
out4
in5 <- -7.06125 +(8.5741*out4)
in5
out5 <- 1/(1+exp(-in5))
out5
#confusing
output=compute(n,training[,-1])
p1=output$net.result
pred1=ifelse(p1>=0.5,1,0)
tab1=table(pred1,training$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
#for test data
output=compute(n,testing[,-1])
p2=output$net.result
pred2=ifelse(p2>=0.5,1,0)
tab2=table(pred2,testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)
#five neuron
n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE)
plot(n) 
#confusing five nodes
output=compute(n,training[,-1])
p1=output$net.result
pred1=ifelse(p1>=0.5,1,0)
tab1=table(pred1,training$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
#for test data
output=compute(n,testing[,-1])
p2=output$net.result
pred2=ifelse(p2>=0.5,1,0)
tab2=table(pred2,testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)

#t neuron
n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = c(2,3),
               err.fct = "ce",
               linear.output = FALSE)
n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = c(2,1),
               err.fct = "ce",
               linear.output = FALSE)
plot(n)
#confusing five nodes
output=compute(n,training[,-1])
p1=output$net.result
pred1=ifelse(p1>=0.5,1,0)
tab1=table(pred1,training$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
#for test data
output=compute(n,testing[,-1])
p2=output$net.result
pred2=ifelse(p2>=0.5,1,0)
tab2=table(pred2,testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)
#five
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign='full',
               rep=5,
               algorithm = "rprop+",
               stepmax = 100000 )
plot(n,rep=1)
plot(n, col.hidden='darkgreen', 
     col.entry.synapse ='red',
     show.weights = T,information =F, fill='lightblue') 


#confusing
output=compute(n,training[,-1],rep=1)
p1=output$net.result
pred1=ifelse(p1>=0.5,1,0)
tab1=table(pred1,training$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
#for test data
output=compute(n,testing[,-1],rep=1)
p2=output$net.result
pred2=ifelse(p2>=0.5,1,0)
tab2=table(pred2,testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)
#bostonhousing
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
data("BostonHousing")
data=BostonHousing
?BostonHousing
str(data)
data%<>% mutate_if(is.factor,as.numeric)
str(data)
n=neuralnet(medv~crim+zn+indus+chas+nox+rm+
              age+dis+rad+tax+ptratio+b+lstat,
            data=data,hidden=c(10,5),
            linear.output=F,lifesign='full',rep=1)
plot(n)
plot(n,col.hidden='darkgreen',
     col.entry.synapse ='red',show.weights = T,
     information =F,fill='lightblue' )
data=as.matrix(data)

dimnames(data)=NULL
set.seed(1234)
ind=sample(2,nrow(data),replace=T,prob=c(.7,.3))
training=data[ind==1,1:13]
test=data[ind==2,1:13]
trainingtarget=data[ind==1,14]
testtarget=data[ind==2,14]
m=colMeans(training)
m
s=apply(training,2,sd)
s
training=scale(training,center=m,scale=s)
test=scale(test,center=m,scale=s)
#create model
install.packages("devtools")
require(devtools)
library(devtools)
devtools::install_github("rstudio/reticulate", force=TRUE)
library(reticulate)
devtools::install_github("r-lib/processx",force=TRUE)
library(processx)
devtools::install_github("rstudio/tensorflow",force=TRUE)
library(tensorflow)
devtools::install_github("rstudio/keras",force=TRUE)
library(keras)
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 5, activation='relu',
              input_shape = c(13)) %>% 
  layer_dense(units = 1) 
  #layer_activation('softmax')

model %>% compile(loss='mse',
                  optimizer='rmsprop',
                  metrics='mae')
#fit model
mymodel<-model %>% 
  fit(training,
      trainingtarget,
      epochs = 100 ,
      batch_size = 32 ,
      validation_split = 0.2)
summary(mymodel)
model %>% evaluate(test,testtarget)
pred=model%>% predict(test) 
mean((testtarget-pred)^2)
plot(testtarget,pred)
 #fine tune

model <- keras_model_sequential() 
model %>% 
  
  layer_dense(units = 100, activation='relu',input_shape = c(13)) %>% 
  layer_dropout(rate=0.4)%>%
  layer_dense(units = 50, activation='relu') %>% 
  layer_dropout(rate=0.3)%>%
  layer_dense(units = 20, activation='relu') %>% 
  layer_dropout(rate=0.2)%>%
  layer_dense(units = 1) 
  #layer_activation('softmax')
summary(model)
model %>% compile(loss='mse',
            optimizer=optimizer_rmsprop(lr=0.001),
                  metrics='mae')
#fit model
mymodel<-model %>% 
  fit(training,
      trainingtarget,
      epochs = 100 ,
      batch_size = 32 ,
      validation_split = 0.2)
summary(mymodel)
model %>% evaluate(test,testtarget)
pred=model%>% predict(test) 
mean((testtarget-pred)^2)
plot(testtarget,pred)
