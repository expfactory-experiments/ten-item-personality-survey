#' Calculate TIPI scores
#'
#' Calculates TIPI scores.  Expects a data frame
#' formatted by \code{\link[expfactory:process_expfactory_survey]{expfactory::process_expfactory_survey}}, containing
#' responses from a single participant.
#'
#' @param df Data frame
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate rename select tibble
#' @importFrom expfactory reverse_code_survey
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @keywords TIPI
#' @export
#' @return tibble
tipi <- function(df) {
  # From Gosling et al. (2003, Appendix A)
  # R denotes reverse-scored items, but these are already reversed in survey.tsv, so no need to do that here
  # Openness to Experiences: 5, 10R
  # Conscientiousness: 3, 8R
  # Extraversion: 1, 6R
  # Agreeableness: 2R, 7
  # Emotional Stability: 4R, 9
  df <- df %>% mutate(value = as.integer(.data$value))

  ## Factors
  score_scale <- function(df, questions, items) {
    score <- df %>%
      filter(grepl(paste(questions, collapse="|"), .data$question)) %>%
      select(.data$value) %>%
      mutate(item = items) %>%
      pivot_wider(names_from = .data$item, values_from = .data$value, names_prefix = 'q')
    score %>% mutate(score = rowSums(score))
  }
  o <- score_scale(df, c("Open to new experiences, complex.", "Conventional, uncreative."), c(5, 10)) %>%
    rename(o = .data$score)
  c <- score_scale(df, c("Dependable, self-disciplined.", "Disorganized, careless."), c(3, 8)) %>%
    rename(c = .data$score)
  e <- score_scale(df, c("Extraverted, enthusiastic.", "Reserved, quiet."), c(1, 6)) %>%
    rename(e = .data$score)
  a <- score_scale(df, c("Critical, quarrelsome. ", "Sympathetic, warm."), c(2, 7)) %>%
    rename(a = .data$score)
  es <- score_scale(df, c("Anxious, easily upset.", "Calm, emotionally stable."), c(4, 9)) %>%
    rename(es = .data$score)
  
  tibble(o, c, e, a, es)
}
