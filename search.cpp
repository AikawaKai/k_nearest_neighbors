#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void convolveCpp(NumericMatrix training, NumericMatrix test) {
  //int na = a.size(), nb = b.size();
  int numRowTr = training.rows();
  int numColTr = training.cols();
  int numRowTe = test.rows();
  int numColTe = test.cols();
  std::cout<<numRowTr;
  std::cout<<"\n";
  std::cout<<numColTr;
  for (int i=0;i<numRowTr;i++)
  {
    for (int j=0;j<numColTr;j++)
    {
      std::cout<<training.row(i).operator[](j);
      std::cout<<" ";
    }
    std::cout<<"\n";
  }
  //int numCol = training.ncol();
}
