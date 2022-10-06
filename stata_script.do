*start by importing the clean .csv from R Studio

*summary for the focal variables
sum ad rd mkt sales assets

*indexing the panel
xtset id year

*generating the additional variables (eg. lags)
gen l_assets = log(assets)
gen l_sales = log(sales)
gen l_earnings = log(earnings)
gen lag_rd = l.rd
gen ass_t = sales/assets


*fixed effect model
xtreg ass_t sov lag_rd l_assets hhi_index threat l_earnings, fe
estimates store m1_fe

*random effect model
xtreg ass_t sov lag_rd l_assets hhi_index threat l_earnings, re
estimates store m1_re

*hausman test
hausman m1_fe m1_re
*the null is rejected, i.e. the FE model is consistent

xtreg fv sov ads c.sov#c.ads l_assets lag_rd hhi_index threat, fe

sum ads, det
margins, dydx(sov) at(ads=(0 0.001 0.002 0.005 0.014 0.043 0.107 0.219 0.808))
marginsplot, ciopt(color(blue%10)) recastci(rarea) title("Margins Plot") xtitle("Advertisement Intensity") ytitle("Marginal Effect of Share of Voice") graphregion(ifcolor(white) fcolor(white) ilcolor(white) margin(r+8)) xlabel(,nolabels)
