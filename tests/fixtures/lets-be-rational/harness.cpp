#include <cstdio>
#include <cstring>
#include "normaldistribution.h"
extern "C" double normalised_black_call(double x, double s);
extern "C" double normalised_vega(double x, double s);
extern "C" double black(double F, double K, double sigma, double T, double q);
extern "C" double implied_volatility_from_a_transformed_rational_guess(double price, double F, double K, double T, double q);
int main(){
  char op[32];
  while (scanf("%31s", op)==1){
    if (!strcmp(op,"nbc")){ double x,s; scanf("%lf %lf",&x,&s); printf("%.17g\n", normalised_black_call(x,s)); }
    else if (!strcmp(op,"vega")){ double x,s; scanf("%lf %lf",&x,&s); printf("%.17g\n", normalised_vega(x,s)); }
    else if (!strcmp(op,"black")){ double F,K,v,T,q; scanf("%lf %lf %lf %lf %lf",&F,&K,&v,&T,&q); printf("%.17g\n", black(F,K,v,T,q)); }
    else if (!strcmp(op,"iv")){ double p,F,K,T,q; scanf("%lf %lf %lf %lf %lf",&p,&F,&K,&T,&q); printf("%.17g\n", implied_volatility_from_a_transformed_rational_guess(p,F,K,T,q)); }
    else if (!strcmp(op,"erfcx")){ double x; scanf("%lf",&x); printf("%.17g\n", erfcx_cody(x)); }
    else if (!strcmp(op,"ncdf")){ double x; scanf("%lf",&x); printf("%.17g\n", norm_cdf(x)); }
  }
}
