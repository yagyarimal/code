install.packages("BiocManager") 
BiocManager::install("EBImage") 
library(EBImage)
install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras)
#library(kerasR)
setwd('C:/Users/yagyarimal/Desktop/publication/Image')
pics=c('p1.jpg','p2.jpg','p3.jpg','p4.jpg','p5.jpg','p6.jpg','c1.jpg','c2.jpg','c3.jpg','c4.jpg','c5.jpg','c6.jpg')
pics
mypic=list()
for(i in 1:12){ mypic[[i]]=readImage(pics[i])}
print(mypic[[1]])
display(mypic[[1]])
display(mypic[[8]])
summary(mypic[[1]])
hist(mypic[[8]])
str(mypic)
# size resizig 
for(i in 1:12){ mypic[[i]] <- resize(mypic[[i]],28, 28)}
28*28*3
str(mypic)
for(i in 1:12){ mypic[[i]]<-array_reshape(mypic[[i]],c(28,28,3))}
# 28*28*3 single vector
str(mypic)
trainx=NULL 
for(i in 1:5){trainx=rbind(trainx,mypic[[i]])}# independent for plane
str(trainx)
for(i in 7:11){ 
trainx=rbind(trainx,mypic[[i]])}# independent for car
str(trainx)
testx=rbind(mypic[[6]],mypic[[12]])# independent varialbes test
28*28*3
trainy=c(0,0,0,0,0,1,1,1,1,1) # dependent variables 
testy=c(0,1) #dependent varaible test
library(keras)
#tensorflow::install_tensorflow()
#install_keras()#after anocanda setup
library(tidyverse)
trainlabls=to_categorical(trainy)
testlabls=to_categorical(testy)
trainlabls 
testlabls
model=keras_model_sequential()
model%>%
  layer_dense(units=256,activation = 'relu',input_shape = c(2352))%>%
  layer_dense(units=128,activation = 'relu')%>%
  layer_dense(units=2,activation = 'softmax')
summary(model)
 2352*256+256
 256*128+128
 128*2+2
 model%>%
   compile(loss='binary_crossentropy',
           optimizer=optimizer_rmsprop(),
            metrics=c('accuracy'))
history<-model %>%
   fit(trainx,
       trainlabls,
       epochs=30, 
       batch_size=32,
       validation_split=0.2)
 plot(history)
model %>% evaluate(trainx,trainlabls)
pred=model%>% predict_classes(trainx)
pred
table(Predicted=pred,Actual=trainy)
prob=model %>% predict_proba(trainx)
prob
cbind(prob,Predicted=pred,Actual=trainy )

 
