#' Announces the number of candygrams for a person.
#'
#' @param person The candygram recipient
#' @param number How many grams they got
#' @param extra_message A string giving extra commentary.
#'
#' @return A candy gram announcement
#'
#' @importFrom stringr str_detect str_to_title
#' @importFrom english as.english
#'
#' @export
give_candygrams <- function(person, number,
                            extra_message = NULL) {

  stopifnot(number > 0)

  if (str_detect(person, "Gretchen")) {

    return(cat("None for Gretchen Weiners."))

  }

  if (is.null(extra_message)) {

    extra_message <- add_commentary(person, number)

  }

  number <- str_to_title(as.english(number))


  glue::glue("{number} for {person}.{extra_message}")



}

#' Tacks commentary on to candygram announcement
#'
#' @param person The candygram recipient
#' @param number How many grams they got
#'
#' @return A string (possibly blank)
add_commentary <- function(person, number) {

  if (stringr::str_detect(person, "Aaron")) {

    return("They are from Regina.")

  }


  if (number > 3) {

    return(glue::glue("You go, {person}!"))

  }


  return("")

}

#' Announces the number of candygrams for people.
#'
#' @param people A vector of candygram recipients
#' @param numbers A vector of how many grams they got
#' @param extra_messages An optional vector giving extra commentary.
#'
#' @return A vector of candy gram announcements
#'
#' @importFrom stringr str_detect str_to_title
#' @importFrom english as.english
#' @importFrom purrr map_chr
#'
#' @export
give_many_candygrams <- function(people, numbers, extra_messages = rep(NA, length(people))) {
  stopifnot(all(numbers > 0))

  if (any(is.na(extra_messages))) {
    extra_messages[is.na(extra_messages)] <- mapply(add_commentary, people[is.na(extra_messages)], numbers[is.na(extra_messages)])
  }


  announcements <- mapply(function(person, number, extra_message) {
    if (stringr::str_detect(person, "Gretchen")) {
      return("None for Gretchen Weiners.")
    } else {
      number_word <- stringr::str_to_title(english::as.english(number))
      if (extra_message != "") {
        return(glue::glue("{number_word} for {person}. {extra_message}"))
      } else {
        return(glue::glue("{number_word} for {person}."))
      }
    }
  }, people, numbers, extra_messages, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  return(unlist(announcements))
}
