library(HeartR)

test_that("summ_heartr has a valid output", {
  expect_s3_class(summ_heartr(heart_dat$age), "summ_heartr")
  expect_length(summ_heartr(heart_dat$restingBP), 9)
  expect_identical(summ_heartr(heart_dat$age)$Mean, mean(heart_dat$age))
  expect_identical(summ_heartr(heart_dat$oldpeak)$Median, median(heart_dat$oldpeak))
  expect_identical(summ_heartr(heart_dat$cholesterol)$Var, var(heart_dat$cholesterol))
  })


test_that("summ_heartr only takes a numeric input", {
  expect_error(summ_heartr(heart_dat))
  expect_error(summ_heartr(heart_dat$STslope))
  expect_error(summ_heartr(heart_dat$restingBP, heart_dat$fastingBS))
})

