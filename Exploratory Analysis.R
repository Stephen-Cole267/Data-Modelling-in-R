set.seed(7045607)
sampid = sample(dim(Boston)[1],400)
BostonNew = Boston[sampid,]
dim(BostonNew)
Boston[1]

head(BostonNew,3)
save(BostonNew,file = "BostonNew.RData")
load("BostonNew.RData")

#Scatter plot matrix of pairs
pairs(BostonNew)
m = colMeans(BostonNew)

#Sample variances
sv = apply(BostonNew, 2, var)

#Sample covariance matrix
cv = var(BostonNew)

#Sample correlation matrix
corr = cor(BostonNew)
corr

#Heatmap where white/yellow implies high values and red/orange is low values
heatmap(t(data.matrix(BostonNew)),Rowv = NA, Colv = NA, labRow = NA)

S = var(BostonNew)
s_sq = diag(S)
total_variation = sum(s_sq)
total_variation

#Means of each variable before any sacaling
m


names(BostonNew)

#Standard Deviations of unscaled data
sqrt(sv)
