library(testthat)
library(shiva)
shiva::set_wfdb_path("/usr/local/bin")

test_check("shiva")
