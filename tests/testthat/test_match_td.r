
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

test_that("matching on time-fixed and time-dependent variable", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac + meds,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza")

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1,], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$pair_id)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(45, 45))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 0)
})

test_that("using replace_at_t and replace_over_t", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  replace_at_t=TRUE,
                  replace_over_t=TRUE)
  expect_equal(max(table(out$.id)), 3)
})

test_that("using 10 controls per case, replace_at_t=TRUE", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  replace_at_t=TRUE,
                  replace_over_t=FALSE,
                  ratio=10,
                  keep_all_columns=TRUE,
                  if_lt_n_at_t="nothing")

  # replacement only took place at the same t
  out_untreated <- subset(out, !.treat)

  check <- out_untreated[, .(var = stats::var(.treat_time)), by=.id]
  expect_true(all(is.na(check$var) | check$var==0))

  # only as much pair_ids as treated
  expect_true(length(unique(out$pair_id))==229)
})

test_that("using 10 controls per case, replace_over_t=TRUE", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  replace_at_t=FALSE,
                  replace_over_t=TRUE,
                  ratio=10,
                  keep_all_columns=TRUE,
                  if_lt_n_at_t="nothing")

  # replacement only took place at the same t
  out_untreated <- subset(out, !.treat)

  check <- out_untreated[, .(var = stats::var(.treat_time)), by=.id]
  expect_true(all(is.na(check$var) | check$var!=0))

  # only as much pair_ids as treated
  expect_true(length(unique(out$pair_id))==229)
})

test_that("using 10 controls per case, replace_cases=FALSE", {

  d_single2 <- as.data.frame(d_single)

  set.seed(134)
  out <- match_td(formula=vacc ~ mac,
                  data=d_single2,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  replace_at_t=FALSE,
                  replace_over_t=FALSE,
                  replace_cases=FALSE,
                  ratio=10,
                  keep_all_columns=TRUE,
                  if_lt_n_at_t="nothing")

  # a lot less cases than usual
  expect_true(length(unique(out$pair_id))==86)
})

test_that("output of match_td() and match_td.fit() is equal", {

  ## when matching on time-fixed variables
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

  ## when matching on time-dependent variables as well
  set.seed(134)
  out1 <- match_td(formula=vacc ~ mac + meds,
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
                       match_vars=c("mac", "meds"))

  expect_equal(out1, out2)
})

test_that("using matchit", {

  set.seed(134)
  out <- match_td(formula=vacc ~ mac + meds,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  event="influenza",
                  use_matchit=TRUE)

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1,], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$pair_id)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(40, 40))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 0)
})
