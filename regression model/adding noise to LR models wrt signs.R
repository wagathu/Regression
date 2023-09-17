
# Date: 09/02/2021
# Credits: stanley sayianka
# tags: ML, kNN, classifier, regression, MSE
# this model is used to add noise to linear regression predicted output
# based on the KNN algorithm, but with attention to regression line
# Attention to regression lines implies keen attention to
# sign of residual

combine_knnlm<-function(train, test, train_labs, k, wrt_sign=c(T, F))
{
  require(compiler)
  # train - train dataset
  # test - test dataset
  # train_labs - train labels(continuous labels for regression)
  # k - the number of nearest neighbours
  # wrt_sign - Paying keen respect to residuals signs
  
  # ensuring ncol(test) = ncol(train)
  if (ncol(train)!=ncol(test))
  {
    stop("Number of columns for both train and test dataset should be equal")
  }
  
  else
  {
    
    dhfn<-function(x,y)
    { ## distance holder function
      #x,y are MATRICES
      #x-test data
      #y-train data
      z<-data.frame()
      x<-as.matrix(x)
      y<-as.matrix(y)
      for (i in 1:nrow(x))
      {
        for (j in 1:nrow(y))
        {
          z[j,i]<-sqrt(sum((x[i, ]-y[j, ])**2)) # euclidean dist
        }
      }
      return(z)
    }
    # speeding up with compiler library
    cmpdhfn <- cmpfun(dhfn)
    
    distance_holder<-data.frame()
    # a dataframe to hold distances, col-test, row-train
    # the columns in distance holder are the test rows
    # the row names in distance holder are the train rows
    distance_holder<-cmpdhfn(test,train)
    
    # a list to hold the orders of distances
    distance_list<-list()
    
    # a list to hold the nearest labels
    labels_holder<-list()
    cls <- vector()
    
    for (i in 1:ncol(distance_holder))
    {
      distance_list[[i]]<-order(distance_holder[, i])[1:k]
    }
  }
  
  return(distance_list)
}
