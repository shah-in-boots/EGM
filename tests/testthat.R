library(testthat)
library(EGM)
EGM::set_wfdb_path("/usr/local/bin")

test_check("EGM")
