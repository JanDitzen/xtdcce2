clear

*Requires testdataset, available at https://www.dropbox.com/s/0087vh8brhid5ws/xtdcce2_sample_dataset.dta?dl=0
use xtdcce2_sample_dataset.dta, clear

**xtdcce2
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross reportc
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(_none)
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd)
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd) cr_lags(0)
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd) cr_lags(3)
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd) cr_lags(3)
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd) cr_lags(3)
xtdcce2 d.log_rgdpo d.L.log_rgdpo d.log_hc d.log_ck d.log_ngd , cr(_all) reportc lr(L.log_rgdpo log_hc log_ck log_ngd) p(L.log_rgdpo log_hc log_ck log_ngd)
xtdcce2 d.log_rgdpo d.L.log_rgdpo d.log_hc d.log_ck d.log_ngd , cr(_all) reportc lr(L.log_rgdpo log_hc log_ck log_ngd) p(L.log_rgdpo log_hc log_ck log_ngd) lr_options(nodivide)
xtdcce2 d.log_rgdpo d.L.log_rgdpo d.log_hc d.log_ck d.log_ngd , cr(_all) reportc lr(L.log_rgdpo log_hc log_ck log_ngd) p(L.log_rgdpo log_hc log_ck log_ngd) lr_options(xtpmgnames)
xtdcce2 d.log_rgdpo d.L.log_rgdpo d.log_hc d.log_ngd (d.log_ck = L.log_ck), cr(_all) reportc 
predict res, res
predict coeff, coeff
*xtcd2
reg d.log_rgdpo log_hc log_ck log_ngd
xtcd2

drop res
reg d.log_rgdpo log_hc log_ck log_ngd
predict res, residuals
xtcd2 res
xtcd2 res, kdensity
xtcd2 log_rgdpo, noestimation
