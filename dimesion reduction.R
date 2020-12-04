data ("iris")
str (iris)
summary(iris)
set.seed (111)
ind = sample (2, nrow ( iris ) , replace = TRUE,  prob = c (0.8, 0.2))
training =iris [ind == 1,]
test=iris[ind == 2,]
library(psych)
pairs.panels(training[,-5],
             gap=0,
             bg=c("red","yellow","blue")[training $Species],
             pch=21) 
pc= prcomp (training[,-5],center = TRUE, scale. = TRUE) 
print(pc)
summary (pc)
pc$scale
plot(pc)
pc$x
pairs.panels (pc$x,gap=0,bg=c("red","yellow","blue")[training $ Species ], pch = 21)
library (devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g = ggbiplot(pc,obs.scale  = 1, 
               var.scale = 1, 
               groups = training$Species, 
               ellipse = TRUE,
               circle = TRUE, 	
               ellipse.prob = 0.68)
g =  g+ scale_color_discrete(name='')
g =  g+theme(legend.direction='horizontal',legend.position='top')
plot(g) 
g = ggbiplot (pc,obs.scale  = 1, 
              var.scale = 1, 
              groups = training$Species, 
              ellipse = TRUE,
              circle = TRUE, 	
              ellipse.prob = 0.95)
plot(g)
trg = predict (pc,  training)
trg = data.frame (trg, training [5])
trg
tr = predict (pc, test)
tr = data.frame (tr, test)
tr = data.frame (tr, test$Species)
tr
library (nnet)
trg$Species = relevel (trg$Species, ref = "setosa")
mymodel = multinom (Species~ PC1+ PC2,data = trg) 
summary(mymodel)
p =predict(mymodel,trg)
tab = table (p, trg $ Species)
tab
1-sum ( diag ( tab ) / sum ( tab ) )
p1 = predict (mymodel, tr   )
tab1 = table (p1, tr $ Species)
1-sum ( diag ( tab1 ) / sum ( tab1 ) )
#PLS Model

data=Hitters
data
summary(Hitters)
library(plsdepot)
library(pls)
set.seed (111)
p<-pls::plsr( Salary ~. , data =data, scale = TRUE,  validation = "CV")
summary (p)
validationplot(p)
set.seed (4)
p<-pls::plsr( Salary ~. , data =data, scale = TRUE,  validation = "CV")
summary (p)
validationplot(p)
attributes(p)
p$coefficients
head(data)

