test_that("join_msi_locus_tables works", {

  locus_section <- read_msi_locus_section("test_data/clc_msi_report.xlsx")

  locus_section_mutate <- mutate_msi_row_numbers(locus_section)

  locus_section_joined <- join_msi_locus_tables(locus_section_mutate)

  expect_true(anyNA.data.frame(locus_section_joined)==FALSE)

  expect_equal(dim(locus_section_joined), c(369, 5))

})

