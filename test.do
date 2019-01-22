clear all

adopath ++ "C:\Users\IPEjd219\Google Drive\Research\StataCode\xtdcce2\xtdcce2_current version\"

cd "C:\Users\IPEjd219\Google Drive\Research\StataCode\xtdcce2\"

use xtdcce2_sample_dataset.dta, clear


xtdcce2 d.log_rgdpo log_hc log_ck, nocross 

predict res, res

xtcd res
