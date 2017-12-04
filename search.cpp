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

int classify(NumericMatrix training, std::multimap<double,int> res){
  std::map<double,int>::iterator it = res.begin();
  int count_1 = 0;
  int count_2 = 0;
  NumericVector nearestClasses = NumericVector(7);
  for(int j=0;j<7;j++)
  {
    double new_dist = it->first;
    int index = it->second;
    NumericVector train_j = training.row(j);
    int class_train = train_j.operator()(14);
    if(class_train==2){
      std::cout<<class_train;
      std::cout<<" ";
    }
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

// [[Rcpp::export]]
NumericMatrix searchCpp(NumericMatrix training, NumericMatrix test) {
  //int na = a.size(), nb = b.size();
  int numRowTr = training.rows();
  int numColTr = training.cols();
  int numRowTe = test.rows();
  int numColTe = test.cols();
  std::cout<<numRowTr;
  std::cout<<"\n";
  std::cout<<numColTr;
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
    int class_ = classify(training, orderingMap);
    /*std::cout<<class_;
    std::cout<<"\n";*/
  }
}
