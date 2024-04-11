#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int SumSingle(List modelList, NumericVector testData) {
  int ncol = modelList.size();
  int cols = ncol / 2;
  double pred = 0;

  CharacterVector datum_names = testData.names();

  for (int j = 0; j < ncol; ++j) {
    List model = modelList[j];
    double z = as<double>(model["(Intercept)"]);
    CharacterVector predictor_names = model.names();

    for (int k = 0; k < predictor_names.size(); ++k) {
      std::string coef_name = as<std::string>(predictor_names[k]);
      if (coef_name != "Intercept") {
        double coefficient = as<double>(model[coef_name]);
        int idx = std::distance(datum_names.begin(), std::find(datum_names.begin(), datum_names.end(), coef_name));
        if (idx < datum_names.size()) {
          z += coefficient * testData[idx];
        }
      }
    }

    pred += z;
  }

  return pred > cols ? 1 : 0;
}
