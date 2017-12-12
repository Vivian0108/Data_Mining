a2_data=read.csv("E:/Acad_9th_Sem/ME781_Data_Mining/Assignment/Assgn1/a2-data-set.csv",header=TRUE)
x1 = a2_data[1:1000,1]
x2 = a2_data[1:1000,2]
y = a2_data[1:1000,3]
mydf = data.frame(x1,x2,y)

set.seed(1)
sindex =sample.int(1000,40)
mydf.s1 = data.frame(x1 = mydf$x1[sindex],x2 = mydf$x2[sindex], y=mydf$y[sindex])

ctrl = trainControl(method="CV", number=10, returnResamp = "all")

lm1 = lm(mydf.s1$y ~ mydf.s1$x1 + mydf.s1$x2)

tdf = data.frame(mydf.s1$x1,mydf.s1$x2)
tr.lm = train(as.matrix(tdf), mydf.s1$y, method="lm", trControl=ctrl)
tr.svmRadial = train(as.matrix(tdf), mydf.s1$y, method="svmRadial", trControl=ctrl)
tr.svmLinear = train(as.matrix(tdf), mydf.s1$y, method="svmLinear", trControl=ctrl)
tr.ridge = train(as.matrix(tdf), mydf.s1$y, method="ridge", trControl=ctrl)
tr.lasso = train(as.matrix(tdf), mydf.s1$y, method="lasso", trControl=ctrl)
tr.enet = train(as.matrix(tdf), mydf.s1$y, method="enet", trControl=ctrl)

tr_obj = list(lm1,tr.lm,tr.svmRadial,tr.svmLinear,tr.ridge,tr.lasso,tr.enet)
ypred = rep(0,7)
cor1 = rep(0,7)

ypred1 = predict(tr.enet)

#ypred[i] = predict(tr_obj[i])
plot(mydf.s1$y,ypred1)
plot(ypred1,mydf.s1$y - ypred1)
cor1 = cor(ypred1,mydf.s1$y)

#t = order(mydf.s1$x1)
#plot(mydf.s1$x1[t],y[t])
#lines(mydf.s1$x1[t], ypred1[t], col="red")
rmse1 = sqrt(mean((ypred1-mydf.s1$y)^2))
rmse1
mae1 = mean(abs(ypred1-mydf.s1$y))
mae1
lm1