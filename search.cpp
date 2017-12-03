#include <Rcpp.h>
using namespace Rcpp;

double distance(NumericVector x1, NumericVector x2){
  int vectorLen = x1.size();
  double sum = 0;
  for(int i=0;i<vectorLen-1;i++){
    sum = sum + pow(x1.operator()(i),2) + pow(x2.operator()(i),2);
  }
  return sqrt(sum);
  
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
    std::map<double, int> orderingMap;
    for (int j=0;j<numRowTr;j++){
      NumericVector train_j = training.row(j);
      double dist = distance(test_i, train_j);
      orderingMap[dist] = j;
    }
    std::map<double,int>::iterator it = orderingMap.begin();
    for(int j=0;j<7;j++)
    {
      double new_dist = it->first;
      int index = it->second;
      std::cout << "value: ";
      std::cout << new_dist;
      std::cout << " index:";
      std::cout << index;
      std::cout <<"\n";
      it++;
    }
    std::cout<<"\n##########\n\n";
  }
}
