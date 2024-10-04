
set.seed(12314)

dat <- data.table(
  id=seq_len(300),
  A=sample.int(n=2, size=300, replace=TRUE),
  B=sample.int(n=2, size=300, replace=TRUE),
  treatment=as.logical(sample.int(n=2, size=300, replace=TRUE,
                                  prob=c(0.8, 0.2))-1)
)
dat[, strat := paste0(A, B)]

# TODO:
#  - fails if strata is named "strata", probably has similar issues with
#    other variable names. Check everywhere, write test for it

test_that("1:1 matching without replacement", {
  set.seed(123431)
  out <- fast_exact_matching(data=dat, treat="treatment", strata="strat",
                             replace=FALSE, ratio=1)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*2)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,])
  expect_equal(tabB[1,], tabB[2,])

  # no duplicate id
  expect_true(max(table(out$id))==1)

  # pair_id correctly assigned
  expect_true(all(table(out$pair_id)==2))
})

test_that("1:1 matching with replacement", {
  set.seed(123431)
  out <- fast_exact_matching(data=dat, treat="treatment", strata="strat",
                             replace=TRUE, ratio=1)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*2)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,])
  expect_equal(tabB[1,], tabB[2,])

  # no duplicate id
  expect_true(max(table(out$id))!=1)

  # pair_id correctly assigned
  expect_true(all(table(out$pair_id)==2))
})

test_that("2:1 matching without replacement", {
  set.seed(123431)
  out <- fast_exact_matching(data=dat, treat="treatment", strata="strat",
                             replace=FALSE, ratio=2)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*3)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,]*2)
  expect_equal(tabB[1,], tabB[2,]*2)

  # no duplicate id
  expect_true(max(table(out$id))==1)

  # pair_id correctly assigned
  expect_true(all(table(out$pair_id)==3))
})

test_that("2:1 matching with replacement", {
  set.seed(123431)
  out <- fast_exact_matching(data=dat, treat="treatment", strata="strat",
                             replace=TRUE, ratio=2)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*3)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,]*2)
  expect_equal(tabB[1,], tabB[2,]*2)

  # no duplicate id
  expect_true(max(table(out$id))!=1)

  # pair_id correctly assigned
  expect_true(all(table(out$pair_id)==3))
})

test_that("4:1 matching with replacement", {
  set.seed(123431)
  out <- fast_exact_matching(data=dat, treat="treatment", strata="strat",
                             replace=TRUE, ratio=4)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*5)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,]*4)
  expect_equal(tabB[1,], tabB[2,]*4)

  # no duplicate id
  expect_true(max(table(out$id))!=1)

  # pair_id correctly assigned
  expect_true(all(table(out$pair_id)==5))
})
