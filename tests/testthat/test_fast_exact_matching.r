
set.seed(12314)

dat <- data.table(
  id=seq_len(300),
  A=sample.int(n=2, size=300, replace=TRUE),
  B=sample.int(n=2, size=300, replace=TRUE),
  treatment=as.logical(sample.int(n=2, size=300, replace=TRUE,
                                  prob=c(0.8, 0.2))-1)
)
dat[, strat := paste0(A, B)]

test_that("1:1 matching without replacement", {
  set.seed(123431)
  out <- fast_exact_matching(treatment ~ strat, data=dat, replace=FALSE,
                             ratio=1)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*2)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,])
  expect_equal(tabB[1,], tabB[2,])

  # no duplicate id
  expect_true(max(table(out$id))==1)

  # .id_pair correctly assigned
  expect_true(all(table(out$.id_pair)==2))

  # same output when using formula directly
  set.seed(123431)
  out2 <- fast_exact_matching(treatment ~ A + B, data=dat, replace=FALSE,
                              ratio=1)
  expect_equal(out, out2)

  # works with internal variable names
  set.seed(123431)
  setnames(dat, old=c("treatment", "strat"), new=c("treat", "strata"))
  out3 <- fast_exact_matching(treat ~ strata, data=dat, replace=FALSE,
                              ratio=1)
  setnames(out3, old=c("treat", "strata"), new=c("treatment", "strat"))
  setnames(dat, old=c("treat", "strata"), new=c("treatment", "strat"))
  expect_equal(out, out3)

  # works with non-logical binary treatment variable
  set.seed(123431)
  dat2 <- copy(dat)
  dat2[, treatment := as.numeric(treatment)]
  out4 <- fast_exact_matching(treatment ~ strat, data=dat2, replace=FALSE,
                              ratio=1)
  expect_equal(out, out4)

  # same output with reversed treatment and estimand "ATC"
  dat3 <- copy(dat)
  dat3[, treatment := !treatment]

  set.seed(123431)
  out5 <- fast_exact_matching(treatment ~ strat, data=dat3, replace=FALSE,
                              ratio=1, estimand="ATC")
  out5[, treatment := !treatment]
  expect_equal(out, out5)
})

test_that("1:1 matching with replacement", {
  set.seed(123431)
  out <- fast_exact_matching(treatment ~ strat, data=dat, replace=TRUE, ratio=1)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*2)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,])
  expect_equal(tabB[1,], tabB[2,])

  # no duplicate id
  expect_true(max(table(out$id))!=1)

  # .id_pair correctly assigned
  expect_true(all(table(out$.id_pair)==2))
})

test_that("2:1 matching without replacement", {
  set.seed(123431)
  out <- fast_exact_matching(treatment ~ strat, data=dat, replace=FALSE,
                             ratio=2)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*3)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,]*2)
  expect_equal(tabB[1,], tabB[2,]*2)

  # no duplicate id
  expect_true(max(table(out$id))==1)

  # .id_pair correctly assigned
  expect_true(all(table(out$.id_pair)==3))
})

test_that("2:1 matching with replacement", {
  set.seed(123431)
  out <- fast_exact_matching(treatment ~ strat, data=dat, replace=TRUE, ratio=2)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*3)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,]*2)
  expect_equal(tabB[1,], tabB[2,]*2)

  # no duplicate id
  expect_true(max(table(out$id))!=1)

  # .id_pair correctly assigned
  expect_true(all(table(out$.id_pair)==3))
})

test_that("4:1 matching with replacement", {
  set.seed(123431)
  out <- fast_exact_matching(treatment ~ strat, data=dat, replace=TRUE, ratio=4)

  # one match per treated
  expect_true(nrow(out)==nrow(dat[treatment==TRUE])*5)

  # A and B equally distributed among treated / untreated
  tabA <- table(out$treatment, out$A)
  tabB <- table(out$treatment, out$B)
  expect_equal(tabA[1,], tabA[2,]*4)
  expect_equal(tabB[1,], tabB[2,]*4)

  # no duplicate id
  expect_true(max(table(out$id))!=1)

  # .id_pair correctly assigned
  expect_true(all(table(out$.id_pair)==5))
})
