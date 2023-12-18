test_that("Check create_pred_rds works", {
  cfl <- list_covar_testdata(covar_folder = "../testdata/")
  # should work
  expect_no_error(create_pred_rds(borders_path = "../testdata/rtp_counties.shp",
                                  covar_files_list = cfl,
                                  output_path = "../testdata/"))
})
