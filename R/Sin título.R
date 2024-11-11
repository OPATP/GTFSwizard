rm(list = ls()); gc()

roxygen2::roxygenise()
roxygen2::roxygenise(clean = T)

tools::resaveRdaFiles("data/for_bus_gtfs.rda", compress = "xz")
tools::resaveRdaFiles("data/for_rail_gtfs.rda", compress = "xz")

devtools::check()
devtools::check(remote = T)
devtools::check(remote = T, force_suggests = T, run_dont_test = T)
devtools::check_win_devel()
devtools::check_win_release()
devtools::test()
devtools::submit_cran()

rhub::rhub_setup()
rhub::rhub_doctor()
rhub::rhub_check()
