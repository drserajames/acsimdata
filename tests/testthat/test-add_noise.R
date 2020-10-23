test_that("noises sum correctly", {
  m <- map_maker_random(5,5,10)
  n <- add_noise(m)
  expect_equal(n$total_noise, n$titre_noise+n$antigen_noise+n$serum_noise)
  expect_equivalent(n$total_noise, n$noise_dist_table-n$dist_table)
})

