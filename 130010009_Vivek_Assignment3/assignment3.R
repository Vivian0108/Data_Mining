a3_data=read.csv("E:/Acad_9th_Sem/ME781_Data_Mining/Assignment/Assgn1/a3-data-set.csv",header=TRUE)
x1 = a3_data[1:1440,1]
x2 = a3_data[1:1440,2]
y = a3_data[1:1440,3]
mydf = data.frame(x1,x2,y = as.factor(y))

tdf = data.frame(mydf$x1, mydf$x2)

knn_3 = knn(as.matrix(tdf),as.matrix(tdf), mydf$y, k = 3)
knn_5 = knn(as.matrix(tdf),as.matrix(tdf),mydf$y, k = 5)
svmRadial = svm(as.matrix(tdf), mydf$y, kernel = "radial")
svmLinear = svm(as.matrix(tdf), mydf$y, method="linear")
nnet3 = nnet(y~.,data = mydf,size = 3,decay = 0.01)
nnet5 = nnet(y~.,data = mydf,size = 5,decay = 0.01)
Rforest = randomForest(as.matrix(tdf),mydf$y)

#ypred1 = predict(as.matrix(tdf),as.matrix(tdf),knn_3,k = 3)
#ypred2 = predict(as.matrix(tdf),as.matrix(tdf),knn_5,k = 5)
#ypred1 = predict(nnet3,as.matrix(tdf),type = "class")
#ypred2 = predict(nnet5,as.matrix(tdf),type = "class")
#ypred3 = predict(tr.svmRadial)
#ypred4 = predict(tr.nnet3,as.matrix(tdf))
#ypred5 = predict(tr.nnet5,as.matrix(tdf))
#ypred6 = predict(tr.Rforest)
#ypred7 = predict(tr.svmLinear)

ctrl = trainControl(method="CV", number=10, returnResamp = "all")

grid_knn3 = expand.grid(k = c(3))
grid_knn5 = expand.grid(k = c(5))

tr.knn3 = train(as.matrix(tdf),mydf$y , method = "knn", trControl = ctrl, tuneGrid = grid_knn3)
tr.knn5 = train(as.matrix(tdf),mydf$y , method = "knn", trControl = ctrl, tuneGrid = grid_knn5)

grid_nnet3 = expand.grid(size = c(3), decay = c(0.01))
grid_nnet5 = expand.grid(size = c(5), decay = c(0.01))

tr.nnet3 = train(y~.,data = mydf, method = "nnet", trControl = ctrl, tuneGrid = grid_nnet3)
tr.nnet5 = train(y~.,data = mydf, method = "nnet", trControl = ctrl, tuneGrid = grid_nnet5)

tr.svmLinear = train(as.matrix(tdf),mydf$y, method = "svmLinear", trControl = ctrl)
tr.svmRadial = train(as.matrix(tdf),mydf$y, method = "svmRadial", trControl = ctrl)

tr.Rforest <- train(as.matrix(tdf),mydf$y, method = "rf", trControl = ctrl)

y_pred1 = predict(tr.nnet5,as.matrix(tdf),type = "raw")


#cfm1 = confusionMatrix(knn_3,mydf$y)
#cfm2 = confusionMatrix(knn_5,mydf$y)
#cfm3 = confusionMatrix(as.factor(ypred3),mydf$y)
cfm4 = confusionMatrix(y_pred1,mydf$y)
#cfm5 = confusionMatrix(as.factor(ypred2),mydf$y)
#cfm6 = confusionMatrix(as.factor(ypred6),mydf$y)
#cfm7 = confusionMatrix(as.factor(ypred7),mydf$y)
