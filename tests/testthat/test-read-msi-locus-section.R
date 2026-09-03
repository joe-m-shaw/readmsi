test_that("read_clc_msi_locus_section finds table", {

  df <- read_msi_locus_section("test_data/clc_msi_report.xlsx")

  expect_true(all(dim(df) == c(452, 3)))

})

test_that("errors when section title is incorrect", {

  expect_error(read_msi_locus_section(filepath = "test_data/clc_msi_report.xlsx",
                                      locus_section_title = "wrong title"),
               regexp = "No match for locus section title")

})


