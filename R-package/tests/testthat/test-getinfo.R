context("getinfo")

describe("Invalid inputs", {
  makeActiveBinding("dummy",
    function() structure("Mr. Dummy", class = "xgb.DMatrix"), environment())

  test_that("it errors when a non-simple string is passed as the name", {
    lapply(list(NULL, "", NA_character_, c("a", "b")), function(name) {
      expect_error(getinfo(dummy, name), "must be length 1 character")  
    })
  })
})

