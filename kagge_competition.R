install.packages('viridis')
install.packages("gbm")
install.packages('trees')
library(viridis)
library(gbm)
library(MASS)
library(tree)
library(randomForest)


d.train = read.csv('/Users/robinhuang/Desktop/sta304 302 314/sta314 prediction competition/trainingdata.csv')
d.test = read.csv('/Users/robinhuang/Desktop/sta304 302 314/sta314 prediction competition/test_predictors.csv')

#run 一个linear的full model，去用summary筛选variable
fullmodel=lm(y~.,data=d.train)
summary(fullmodel)

#在result里筛选所有关联性强的predictors（看p-value，我选的是p-value小于0.05的，你可以以更严格的标准来选，但是严格未必是好事）
#我选了x4,x6,x7,x11,x27,x30,x43,x46,x54,x59,x79,x86,x103,x105



train = sample(1:nrow(d.train), nrow(d.train)*0.8)  ##这个0.5可以换，至少0.5以上，比如0.8
test = (-train)

ytrain = d.train$y[train]  ##这个y之后用来fit model
ytest = d.train$y[test]  ##这个y之后用来算error


# basic boosting
# data: as before
# distribution = 'gaussian' is least squares
# n.trees: B in lectures
# interaction.depth: d in lectures
# shrinkage: lambda in lectures
# cv.folds = 5: also do cross-validation


boost.y=gbm(ytrain~X4+X6+X7+X11+X27+X30+X43+X46+X54+X59+X79+X86+X103+X105,
            data=d.train[train, ],
            distribution='gaussian',
            n.trees = 8000,
            interaction.depth = 10,
            shrinkage = 0.003,
            cv.folds = 10
)
#上面的参数要大家自己调整，基本上shrinkage要在0.001到0.01之间，n.trees要在3000-10000之间，当然，shrinkage越小越好，不过越小的shrinkage需要越大的n.trees.
#cv和d大家也自己尝试一个合适的数据

# the next line gives relative variable importance
summary(boost.y)


# next line gives results from cross-validation
bi = gbm.perf(boost.y,method="cv")

bi

#用刚刚找到的bi来进行预测
pr.boost = predict(boost.y,newdata=d.train[-train,],n.trees=bi)

#自行检查error
mean((pr.boost-ytest)^2)
sqrt(mean((pr.boost-ytest)^2))






#还原成所有的trainging set
model=gbm(y~X4+X6+X7+X11+X27+X30+X43+X46+X54+X59+X79+X86+X103+X105,
          data=d.train,
          distribution='gaussian',
          n.trees = 5000,
          interaction.depth = 6,
          shrinkage = 0.01,
          cv.folds = 5
)

bi = gbm.perf(model,method="cv")

bi

#用刚刚找到的bi来进行预测
pr.boost = predict(model,newdata=d.train,n.trees=bi)
pred = predict(model, newdata = d.test)





# now bring in the right format for