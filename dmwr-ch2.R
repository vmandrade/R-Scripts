# code for working thru examples in *Data Mining with R*
# chapter 2 
library(DMwR)

data(algae)
algae = algae[-manyNAs(algae), ]
dim(algae)

# linear regression by model reduction
clean.algae = knnImputation(algae, k=10)
lm.a1 = lm(a1 ~ ., data=clean.algae[, 1:12])
summary(lm.a1)
anova(lm.a1)
lm2.a1 = update(lm.a1, . ~ . - season)
summary(lm2.a1)
anova(lm.a1, lm2.a1)
final.a1 = step(lm.a1)
summary(final.a1)

# regression trees
library(rpart)
rt.a1 = rpart(a1 ~ ., data=algae[, 1:12])
rt.a1
prettyTree(rt.a1)
dev.off()
printcp(rt.a1)
rt2.a1 = prune(rt.a1, cp=.08)
rt2.a1
first.tree = rt.a1
snip.rpart(first.tree, c(4, 7))

# cross-validation
cv.rpart = function(form, train, test, ...){
	m = rpartXse(form, train, ...)
	p = predict(m, test)
	mse = mean((p-resp(form, test))^2)
	c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

cv.lm = function(form, train, test, ...){
	m = lm(form, train, ...)
	p = predict(m, test)
	p = ifelse(p<0, 0, p)
	mse = mean((p-resp(form, test))^2)
	c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}      

res = experimentalComparison(
	c(dataset(a1 ~ ., clean.algae[,1:12])),
	c(variants('cv.lm'),
		variants('cv.rpart', se=c(0, 0.5, 1))),
	cvSettings(3, 10, 1234)
)
summary(res)
plot(res)
dev.off()

DSs = sapply(names(clean.algae)[12:18],
	function(x,names.attrs){
		f = as.formula(paste(x,"~."))
		dataset(f,clean.algae[,c(names.attrs,x)],x)
		},
		names(clean.algae)[1:11]
	)
	

res.all = experimentalComparison(
	DSs,
	c(variants('cv.lm'),
		variants('cv.rpart', se=c(0,0.5,1))
			),
	cvSettings(5,10,1234)
	)

plot(res.all)
dev.off()
bestScores(res.all)

# random forest ensembling
library(randomForest)

cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2)) 
}

res.all = experimentalComparison(
	DSs,
	c(variants('cv.lm'),
		variants('cv.rpart', se=c(0,0.5,1)),
		variants('cv.rf', ntree=c(200,500,700))
		),
	cvSettings(5,10,1234)
	)
bestScores(res.all)
compAnalysis(res.all,against='cv.rf.v3',
	datasets=c('a1','a2','a4','a6'))

bestModelNames = sapply(bestScores(res.all),
	function(x) x['nmse', 'system'])
learners = c(rf='randomForest', rpart='rpartXse')
want = sapply(strsplit(bestModelNames,'\\.'), function(x) x[2])
funcs = learners[want]
parSets = lapply(bestModelNames,
	function(x) getVariant(x, res.all)@pars)
bestModels = list()
for(a in 1:7){
	form = as.formula(paste(names(clean.algae)[11+a],'~.'))
	bestModels[[a]] = do.call(funcs[a],
		c(list(form,clean.algae[,c(1:11,11+a)]), parSets[[a]]))
}

clean.test.algae = knnImputation(test.algae, k=10, 
	distData=algae[,1:11])

preds = matrix(ncol=7, nrow=140)
for(i in 1:nrow(clean.test.algae)){
	preds[i,] = sapply(1:7,
			function(x){
				predict(bestModels[[x]], clean.test.algae[i,])
			}
		)
}
avg.preds = apply(algae[,12:18], 2, mean)
apply( ((algae.sols-preds)^2), 2, mean) /
apply( (scale(algae.sols, avg.preds,F)^2), 2, mean)