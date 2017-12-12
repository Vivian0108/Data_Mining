tab = read.table("E:/Acad_9th_Sem/ME781_Data_Mining/Assignment/130010009_Vivek_Assignment1/a1-data-set.csv",header = TRUE, sep = ",")
plot (tab$x, tab$y)

fit = lm(tab$y ~ tab$x)
summary(fit)
listna = (is.na(tab$y))
tab$y[listna] = summary(fit)$coefficients[2,1]*tab$x[listna] + summary(fit)$coefficients[1,1]

fit1 = lm(tab$y ~ tab$x)
summary(fit1)
ypred = summary(fit)$coefficients[2,1]*tab$x + summary(fit)$coefficients[1,1]
lines(tab$x, ypred, col="red")
rmse = sqrt(mean((ypred-tab$y)^2))
rmse
mae = mean(abs(ypred-tab$y))
mae

plot (tab$y,ypred)
plot (tab$x,(tab$y-ypred))
