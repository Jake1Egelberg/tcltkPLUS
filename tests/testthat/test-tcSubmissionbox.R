library(tcltkPLUS)
library(testthat)

tcTheme("DEFAULT")

#Run test
test_that("Check tcBuild",{
  expect_no_error(
    tcSubmissionbox("test","test","test",self_destruct = TRUE)
  )
  expect_visible(
    tcSubmissionbox("test","test","test",self_destruct = TRUE)
  )
})


