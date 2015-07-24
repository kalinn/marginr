# @file normalize_fns.R
# @author Kristin A Linn
# @date 6-5-15
# @brief Normalize using control group variance or avg of both groups

#################################################################
# This function inputs a feature matrix x and vector of group 
# labels y and returns the control-group normalized x and 
# test set ts
cn.norm = function(x, y, ts){
	cn.x = x[which(y==0),]
	sds = apply(cn.x, 2, sd)
	sc.x = scale(x, center=TRUE, scale=sds)
	center.x = attr(sc.x, "scaled:center")
	sc.ts = scale(ts, center=center.x, scale=sds)
	return(list('sc.x'=sc.x, 'sc.ts'=sc.ts))
} 

#################################################################
# This function inputs a feature matrix x and vector of group 
# labels y and returns the control-group normalized x and 
# test set ts
d.norm = function(x, y, ts){
	d.x = x[which(y==1),]
	sds = apply(d.x, 2, sd)
	sc.x = scale(x, center=TRUE, scale=sds)
	center.x = attr(sc.x, "scaled:center")
	sc.ts = scale(ts, center=center.x, scale=sds)
	return(list('sc.x'=sc.x, 'sc.ts'=sc.ts))
} 

#################################################################
# This function inputs a feature matrix x and vector of group 
# labels y and returns the within-group avg normalized x and 
# test set ts
avg.norm = function(x, y, ts){
	cn.x = x[which(y==0),]
	d.x = x[which(y==1),]
	cn.sd = apply(cn.x, 2, sd)
	d.sd = apply(d.x, 2, sd)
	avg.sd = (cn.sd + d.sd)/2
	sc.x = scale(x, center=TRUE, scale=avg.sd)
	center.x = attr(sc.x, "scaled:center")
	sc.ts = scale(ts, center=center.x, scale=avg.sd)
	return(list('sc.x'=sc.x, 'sc.ts'=sc.ts))
} 

#################################################################
# This function inputs a feature matrix x and test set ts
# and returns the z-score normalized x and ts
z.norm = function(x, ts){
	sc.x = scale(x, center=TRUE, scale=TRUE)
	center.x = attr(sc.x, "scaled:center")
	scale.x = attr(sc.x, "scaled:scale")
	sc.ts = scale(ts, center=center.x, scale=scale.x)
	return(list('sc.x'=sc.x, 'sc.ts'=sc.ts))
} 

#################################################################
# This function inputs a feature matrix x and test set ts
# and returns the z-score normalized x and ts
center.norm = function(x, ts){
	sc.x = scale(x, center=TRUE, scale=FALSE)
	center.x = attr(sc.x, "scaled:center")
	sc.ts = scale(ts, center=center.x, scale=FALSE)
	return(list('sc.x'=sc.x, 'sc.ts'=sc.ts))
} 

#################################################################
# This function inputs a feature matrix x and test set ts
# and returns the range normalized x and ts
range.norm = function(x, ts){
	min.x = apply(x, 2, min)
	range.x = apply(x, 2, function(x) max(x) - min(x))
	sc.x = t(apply(x, 1, function(x) (x - min.x)/range.x))
	sc.ts = t(apply(ts, 1, function(x) (x - min.x)/range.x))
	return(list('sc.x'=sc.x, 'sc.ts'=sc.ts))
} 
 
#############################
# Unadj-SVM
#############################
# must input pre-scaled tr and test data!
fit.svm = function(trX, trY, tsX, cvec){
	testData = as.matrix(tsX)
	colnames(testData) = NULL
	trainData = as.matrix(trX)
	colnames(trainData) = NULL
	trainY = as.vector(trY)
	train.svm = python.call("fit_svm", trainData,
		trainY, testData, cvec)
	return(list('w'=train.svm[[1]],
		'rho'=train.svm[[2]],
		'predicted'=train.svm[[3]],
		'bestC'=train.svm[[4]]))
}

sensitivity = function(est, true){
	correct = est==true
	return(mean(correct[which(true==1)]))
}

specificity = function(est, true){
	correct = est==true
	return(mean(correct[which(true==0)]))
}

###########################
# CLEAN UP THE ADNI DATA  #
###########################
clean = function(dir, diagRm, diag1, lowAge){
	setwd(dir)
	# Read in the data
	# Get Age from old data
	adniOld = read.csv ("ADNI_ROI_volumes.csv", header=T)
	ID = adniOld$INDEX
	idSplit = unlist (lapply (strsplit (as.character (ID), 
		split="_"), 
		function (x) x[3]))

	# Use new data for analysis
	adni = read.csv ("ADNI1_2015_ROI-Volumes_Final.csv", header=T)
	idSplit2 = unlist (lapply (strsplit (as.character (adni$ID), 
		split="_"), function (x) x[3]))
	ind = idSplit%in%idSplit2
	adniOld = adniOld[which (ind==TRUE),]
	adni[,3] = adniOld$Age
	colnames (adni)[3] = "Age"

	# Get rid of age outliers
	adni = adni[which (adni$Age >= lowAge),]
	# adni = adni[-which (adni$Age > 85),]
	# adni = adninew
	icv = adni$X702

	# Get rid of irrelevant features
	adni = adni[,-c(5:118,125,128:134,151:156)]
	# adni = adni[,-c(5:118)]
	# Scale by intra cranial volume
	adni.feat = apply (as.matrix (adni[,-c(1:4)]), 2, 
		function (x, y) x/y, y=icv)
	adni[,-c(1:4)] = adni.feat

	colnames (adni)[4] = "Diagnosis"
	adni = adni[which (adni$Diagnosis!=diagRm),]
	adni$Diagnosis = 1*(adni$Diagnosis==diag1)
	return(adni)
}


