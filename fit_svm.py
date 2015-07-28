import numpy as np
from sklearn import svm, grid_search

def fit_svm(x, y, ts, C):
	#convert to numpy arrays
	nX = np.array(x,ndmin=2)
	ntsX = np.array(ts,ndmin=2)
	nY = np.array(y,ndmin=1)
	dY = nY/1.0
	pos_wt = dY.size/sum(dY)
	neg_wt = 1/(1 - sum(dY)/dY.size)
	#for tuning
	wts = {0: neg_wt, 1: pos_wt}
	paramGrid = [
	{'C': C, 'kernel': ['linear']}
	]
	#train
	trnMdl = svm.SVC(class_weight=wts)
	trnMdl_grid = grid_search.GridSearchCV(trnMdl, paramGrid)
	trnMdl_grid.fit(X=nX, y=nY)
	trnMdl_best = trnMdl_grid.best_estimator_
	#return SVM weights and predictions
	trnMdl_rho = trnMdl_best.intercept_[0:1]
	trnMdl_rho = list(trnMdl_rho)
	trnMdl_w = trnMdl_best.coef_[0,:]
	trnMdl_w = list(trnMdl_w)
	yhats = trnMdl_best.predict(ntsX)
	yhats = list(yhats)
	bestC = trnMdl_grid.best_params_
	return trnMdl_w, trnMdl_rho, yhats, bestC

