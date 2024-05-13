test_that("add_commentary compliments people with lots of grams", {
  correct_result <- "You go, Glen Coco!"

  my_result <- add_commentary("Glen Coco", 4)

  expect_equal(my_result, correct_result)
})



test_that("give_candygrams works", {
  correct_result <- "Two for Taylor Zimmerman."

  my_result <- give_candygrams("Taylor Zimmerman", 2)

  expect_equal(my_result, correct_result)
})

test_that("give_mult_candygrams single recipient without extra message", {
  correct_result <- "One for James."

  my_result <- give_many_candygrams(c("James"), c(1))

  expect_equal(my_result, correct_result)
})

test_that("give_mult_candygrams no candygrams for Gretchen Weiners", {
  correct_result <- "None for Gretchen Weiners."

  my_result <- give_many_candygrams(c("Gretchen Weiners"), c(1))

  expect_equal(correct_result, my_result)
})


test_that("Give give_mult_candygrams vector inputs with different scenarios", {
  correct_result <- c("None for Gretchen Weiners.", "Two for Aaron. They are from Regina.",
                      "Four for Karen. You go, Karen!", "Two for Aditi. Yay!")

  my_result <- give_many_candygrams(c("Gretchen Weiners", "Aaron", "Karen", "Aditi"),
                                    c(1, 2, 4, 2), c(NA, NA, NA, "Yay!"))

  expect_equal( correct_result, my_result)
})
