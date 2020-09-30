library(tidyverse)
library(expfactory)
context('TIPI')

df <- process_expfactory_survey(token='1', survey='../fixtures/1_tipi.json', flat=TRUE) %>%
  rename(p = Token)
test_that("process_expfactory_survey() can process JSON", {
  expect_is(df, 'data.frame')
})

test_that('TIP correctly parsed', {
  expect_equal(df[1,4], 'Extraverted, enthusiastic.')
  expect_equal(df[2,4], 'Critical, quarrelsome. ')
  expect_equal(df[3,4], 'Dependable, self-disciplined.')
  expect_equal(df[4,4], 'Anxious, easily upset.')
  expect_equal(df[5,4], 'Open to new experiences, complex.')
  expect_equal(df[6,4], 'Reserved, quiet.')
  expect_equal(df[7,4], 'Sympathetic, warm.')
  expect_equal(df[8,4], 'Disorganized, careless.')
  expect_equal(df[9,4], 'Calm, emotionally stable.')
  expect_equal(df[10,4], 'Conventional, uncreative.')
})

tipi <- tipi(df)
test_that("tipi() can process JSON", {
  expect_is(tipi, 'data.frame')
})

test_that('TIPI Openness to Experiences value is correct', {
  expect_equal(tipi$o, 12)
})
test_that('TIPI Conscientiousness value is correct', {
  expect_equal(tipi$c, 10)
})
test_that('TIPI Extraversion value is correct', {
  expect_equal(tipi$e, 8)
})
test_that('TIPI Agreeableness value is correct', {
  expect_equal(tipi$e, 8)
})
test_that('TIPI Emotional Stability value is correct', {
  expect_equal(tipi$es, 7)
})