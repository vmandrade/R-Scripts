# code for working thru examples in *Data Mining with R*
# chapter 4

library(DMwR)
load('salesClean.Rdata')
dim(sales)
head(sales)

# precision-recall curve
PRcurve = function(preds, trues, ...){
	require(ROCR, quietly=TRUE)
	pd = prediction(preds, trues)
	pf = performance(pd, "prec", "rec")
	pf@y.values = lapply(pf@y.values, function(x) rev(cummax(rev(x))) )
	plot(pf, ...)
}

data(ROCR.simple)
par(mfrow=c(1,1))
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

# cumulative recall chart
CRchart = function(preds, trues, ...){
	pred = prediction(preds, trues)
	perf = performance(pred, "rec", "rpp")
	plot(perf, ...)
}
CRchart(ROCR.simple$predictions, ROCR.simple$labels,
	main="Cumulative Recall Chart")

# adaboost with rweka
library(RWeka)
WOW(AdaBoostM1)

data(iris)
idx = sample(150, 100)
model = AdaBoostM1(Species ~ ., iris[idx,],
	control=Weka_control(I=100))
preds = predict(model, iris[-idx,])
table(preds, iris[-idx, 'Species'])
prob.preds = predict(model, iris[-idx,], type='probability')

ab = function(train, test){
	require(RWeka, quietly=TRUE)
	sup = which(train$Insp != 'unkn')
	vars = c('ID', 'Prod' ,'Uprice', 'Insp')
	data = train[sup, vars]
	data$Insp = factor(data$Insp, levels=c('ok', 'fraud'))
	model = AdaBoostM1(Insp ~ ., data,
		control=Weka_control(I=100))
	preds = predict(model, test[, vars], type='probability')
	return(list(rankOrder=order(preds[,'fraud'], decreasing=TRUE), 
		rankScore=preds[,'fraud']))
}

ho.ab = function(form, train, test, ...){
	res = ab(train,test)
	structure(evalOutlierRanking(test,res$rankOrder,...),
	itInfo=list(preds=res$rankScore, 
		trues=ifelse(test$Insp=='fraud',1,0) )
	)
}

ab.res = holdOut(learner('ho.ab',
	pars=list(Threshold=0.1, statsProds=globalStats)),
		dataset(Insp ~ ., sales),
		hldSettings(3,0.3, 1234, T),
		itsInfo=TRUE)