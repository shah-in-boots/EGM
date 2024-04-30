library(testthat)
library(egm)
egm::set_wfdb_path("/usr/local/bin")

test_check("egm")
