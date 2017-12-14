#include <Rcpp.h>
using namespace Rcpp;

double distance(NumericVector x1, NumericVector x2){
  int vectorLen = x1.size();
  double sum = 0;
  for(int i=0;i<vectorLen-1;i++){
    sum = sum + pow((x1.operator()(i)-x2.operator()(i)),2);
  }
  return sqrt(sum);
  
}

int classify(NumericMatrix training, std::multimap<double,int> res, int k){
  std::map<double,int>::iterator it = res.begin();
  int count_1 = 0;
  int count_2 = 0;
  NumericVector nearestClasses = NumericVector(k);
  for(int j=0;j<k;j++)
  {
    double new_dist = it->first;
    int index = it->second;
    NumericVector train_j = training.row(index);
    int class_train = train_j.operator()(14);
    nearestClasses(j) = class_train;
    if(class_train==1){
      count_1=count_1+1;
    }else{
      count_2=count_2+1;
    }
  }
  if(count_1>count_2){
    return 1;
  }else{
    return 2;
  }
}

std::string checkTranslator(int check){
  switch(check){
    case 0:
      return "TP";
      break;
    case 1:
      return "FP";
      break;
    case 2:
      return "TN";
      break;
    case 3:
      return "FN";
      break;
    default:
      return "ERROR";
  }
}

int checkRealClass(int class_, int expected){
  switch(class_){
    case 1:
      if (class_ == expected){
        return 0; //TP
      }else{
        return 1; //FP
      }
      break;
    case 2:
      if (class_ == expected){
        return 2; //TN
      }else{
        return 3; //FN
      }
      break;
    default:
      return -1;
  }
}

// [[Rcpp::export]]
NumericVector searchCpp(NumericMatrix training, NumericMatrix test, int k) {
  //int na = a.size(), nb = b.size();
  NumericVector confusionMatrix = NumericVector(4);
  confusionMatrix.operator()(0) = 0;
  confusionMatrix.operator()(1) = 0;
  confusionMatrix.operator()(2) = 0;
  confusionMatrix.operator()(3) = 0;
  int numRowTr = training.rows();
  int numColTr = training.cols();
  int numRowTe = test.rows();
  int numColTe = test.cols();
  /*std::cout<<numRowTr;
  std::cout<<"\n";
  std::cout<<numColTr;*/
  NumericVector AllDistances = NumericVector(numRowTe);
  for (int i=0;i<numRowTe;i++)
  {
    NumericVector test_i = test.row(i);
    std::multimap<double, int> orderingMap;
    for (int j=0;j<numRowTr;j++){
      NumericVector train_j = training.row(j);
      double dist = distance(test_i, train_j);
      orderingMap.insert(std::pair<double,int>(dist,j));
    }
    int class_ = classify(training, orderingMap, k);
    int check = checkRealClass(class_, test_i.operator()(14));
    confusionMatrix.operator()(check) = confusionMatrix.operator()(check)+1;
  }
  return confusionMatrix;
}
