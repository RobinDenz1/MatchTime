
d_single <- readRDS(system.file("testdata",
                                "single_n1000.Rds",
                                package="MatchTD"))

d_multi <- readRDS(system.file("testdata",
                                "multi_n1000.Rds",
                                package="MatchTD"))
d_multi$d_covars[, inclusion := NULL]

test_that("matching on time-fixed variable", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza")

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds not equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_true(tab[1,1] != tab[2,1])

  # pair id always occurs 2 times
  expect_true(all(table(out$pair_id)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(33, 33))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 0)
})

test_that("allowing replacement", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  replace_at_t=TRUE,
                  replace_over_t=TRUE)
  expect_true(max(table(out$.id)), 3)
})

test_that("using 3 controls per case", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  replace_at_t=FALSE,
                  replace_over_t=FALSE,
                  ratio=3,
                  keep_all_columns=TRUE)
  expect_true(max(table(out$.id)), 5)
})

test_that("output of match_td() and match_td.fit() is equal", {

  set.seed(134)
  out1 <- match_td(formula=vacc ~ mac,
                   data=d_single,
                   id=".id",
                   inclusion="inclusion",
                   event="influenza")

  set.seed(134)
  out2 <- match_td.fit(id=".id",
                       time=".time",
                       d_treat=d_multi$d_treat,
                       d_event=d_multi$d_event,
                       d_covars=d_multi$d_covars,
                       match_vars="mac")

  expect_equal(out1, out2)
})
