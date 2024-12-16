
test_that("with 1:1 matching Noah Greifer", {

  # ATT
  data <- data.table(treatment=c(TRUE, FALSE, TRUE, FALSE),
                     .id_pair=c(1, 1, 2, 2))

  set_match_weights(data, treat="treatment", keep_ps=TRUE, estimand="ATT",
                    stabilize=FALSE)
  expect_true(all(data$.ps_score==0.5))
  expect_true(all(data$.weights==1))

  # ATC
  data <- data.table(treatment=c(TRUE, FALSE, TRUE, FALSE),
                     .id_pair=c(1, 1, 2, 2))

  set_match_weights(data, treat="treatment", keep_ps=TRUE, estimand="ATC",
                    stabilize=FALSE)
  expect_true(all(data$.ps_score==0.5))
  expect_equal(data$.weights, c(1, 1, 1, 1))

  # stabilization makes no difference here
  data2 <- data.table(treatment=c(TRUE, FALSE, TRUE, FALSE),
                      .id_pair=c(1, 1, 2, 2))
  set_match_weights(data2, treat="treatment", keep_ps=TRUE, estimand="ATC",
                    stabilize=TRUE)
  expect_equal(data2$.weights, data$.weights)

})

test_that("with 1:k matching Noah Greifer", {

  # ATT
  data <- data.table(.treat=c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                     .id_pair=c(1, rep(1, 7), 2, 2))

  set_match_weights(data, treat=".treat", keep_ps=TRUE, estimand="ATT",
                    stabilize=FALSE)
  expect_equal(data$.ps_score, c(rep(0.125, 8), 0.5, 0.5))
  expect_equal(round(data$.weights, 7), c(1, rep(0.1428571, 7), 1, 1))

  # ATC
  data <- data.table(.treat=c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                     .id_pair=c(1, rep(1, 7), 2, 2))

  set_match_weights(data, treat=".treat", keep_ps=TRUE, estimand="ATC",
                    stabilize=FALSE)
  expect_equal(data$.ps_score, c(rep(0.125, 8), 0.5, 0.5))
  expect_equal(round(data$.weights, 7), c(7, rep(1, 9)))
})

test_that("with 1:k matching Noah Greifer, with stabilization", {

  # ATT
  data <- data.table(.treat=c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                     .id_pair=c(1, rep(1, 7), 2, 2))

  set_match_weights(data, treat=".treat", keep_ps=TRUE, estimand="ATT",
                    stabilize=TRUE)
  expect_equal(data$.ps_score, c(rep(0.125, 8), 0.5, 0.5))
  expect_equal(round(data$.weights, 7), c(1, rep(0.5714286, 7), 1, 4))
  expect_equal(sum(data$.weights[data$.treat]), 2)
  expect_equal(sum(data$.weights[!data$.treat]), 8)

  # ATC
  data <- data.table(.treat=c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                     .id_pair=c(1, rep(1, 7), 2, 2))

  set_match_weights(data, treat=".treat", keep_ps=TRUE, estimand="ATC",
                    stabilize=TRUE)
  expect_equal(data$.ps_score, c(rep(0.125, 8), 0.5, 0.5))
  expect_equal(round(data$.weights, 7), c(1.75, rep(1, 7), 0.25, 1))
  expect_equal(sum(data$.weights[data$.treat]), 2)
  expect_equal(sum(data$.weights[!data$.treat]), 8)
})

## below are some sanity checks

test_that("ATC on reversed .treat is same as ATT on regular, with replace", {

  # ATT on regular data
  data1 <- data.table(.treat=c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                     .id_pair=c(1, rep(1, 7), 2, 2))
  set_match_weights(data1, treat=".treat", keep_ps=TRUE, estimand="ATT",
                    stabilize=FALSE)

  # ATC on reversed .treat data
  data2 <- data.table(.treat=!c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                      .id_pair=c(1, rep(1, 7), 2, 2))
  set_match_weights(data2, treat=".treat", keep_ps=TRUE, estimand="ATC",
                    stabilize=FALSE)

  expect_equal(data1$.weights, data2$.weights)
})

test_that("ATT on reversed .treat is same as ATC on regular, with replace", {

  # ATC on regular data
  data1 <- data.table(.treat=c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                      .id_pair=c(1, rep(1, 7), 2, 2))
  set_match_weights(data1, treat=".treat", keep_ps=TRUE, estimand="ATC",
                    stabilize=FALSE)

  # ATT on reversed .treat data
  data2 <- data.table(.treat=!c(TRUE, rep(FALSE, 7), TRUE, FALSE),
                      .id_pair=c(1, rep(1, 7), 2, 2))
  set_match_weights(data2, treat=".treat", keep_ps=TRUE, estimand="ATT",
                    stabilize=FALSE)

  expect_equal(data1$.weights, data2$.weights)
})
