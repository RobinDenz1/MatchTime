
d_single <- readRDS(system.file("testdata",
                                "single_n1000.Rds",
                                package="MatchTime"))
d_single[, stop := stop + 1]
d_single[, stop := as.integer(stop)]

d_multi <- readRDS(system.file("testdata",
                                "multi_n1000.Rds",
                                package="MatchTime"))
d_multi$d_covars[, inclusion := NULL]
d_multi$d_covars[, stop := stop + 1]
d_multi$d_event[, .time := .time + 1]

test_that("matching on nothing", {

  set.seed(1346)
  out <- match_time(formula=vacc ~ 1,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    match_method="none")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac not equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_true(tab[1,1] != tab[2,2])

  # meds not equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_true(tab[1,1] != tab[2,1])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(26, 26))
  out[, n_id := NULL]

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 26)

  # same output with different names
  setnames(d_single,
           old=c(".id", "start", "stop", "inclusion"),
           new=c("id", "beginning", "end", "incl"))

  set.seed(1346)
  out2 <- match_time(formula=vacc ~ 1,
                     data=d_single,
                     id="id",
                     start="beginning",
                     stop="end",
                     inclusion="incl",
                     match_method="none")$data
  setnames(out2, old="id", new=".id")
  expect_equal(out, out2)

  setnames(d_single,
           new=c(".id", "start", "stop", "inclusion"),
           old=c("id", "beginning", "end", "incl"))
})

test_that("matching on time-fixed variable", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds not equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_true(tab[1,1] != tab[2,1])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(29, 29))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 29)
})

test_that("matching on time-dependent variable", {

  set.seed(13534)
  out <- match_time(formula=vacc ~ meds,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac not equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_true(tab[1,1] != tab[2,1])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1, ], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(49, 49))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 49)
})

test_that("matching on time-fixed and time-dependent variable", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac + meds,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1,], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(40, 40))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 40)
})

test_that("using replace_at_t and replace_over_t", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    replace_at_t=TRUE,
                    replace_over_t=TRUE)$data
  expect_equal(max(table(out$.id)), 3)
})

test_that("using 10 controls per case, replace_at_t=TRUE", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    replace_at_t=TRUE,
                    replace_over_t=FALSE,
                    ratio=10,
                    keep_all_columns=TRUE,
                    if_no_match="nothing")$data

  # replacement only took place at the same t
  out_untreated <- subset(out, !.treat)

  check <- out_untreated[, .(var = stats::var(.treat_time)), by=.id]
  expect_true(all(is.na(check$var) | check$var==0))

  # only as much .id_pairs as treated
  expect_true(length(unique(out$.id_pair))==229)
})

test_that("using 10 controls per case, replace_over_t=TRUE", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    replace_at_t=FALSE,
                    replace_over_t=TRUE,
                    ratio=10,
                    keep_all_columns=TRUE,
                    if_no_match="nothing")$data

  # replacement only took place at the same t
  out_untreated <- subset(out, !.treat)

  check <- out_untreated[, .(var = stats::var(.treat_time)), by=.id]
  expect_true(all(is.na(check$var) | check$var!=0))

  # only as much .id_pairs as treated
  expect_true(length(unique(out$.id_pair))==229)
})

test_that("using 10 controls per case, replace_cases=FALSE", {

  d_single2 <- as.data.frame(d_single)

  set.seed(134)
  out <- match_time(formula=vacc ~ mac,
                    data=d_single2,
                    id=".id",
                    inclusion="inclusion",
                    replace_at_t=FALSE,
                    replace_over_t=FALSE,
                    replace_cases=FALSE,
                    ratio=10,
                    keep_all_columns=TRUE,
                    if_no_match="nothing")$data

  # a lot less cases than usual
  expect_true(length(unique(out$.id_pair))==88)
})

test_that("output of match_time() and match_time.fit() is equal", {

  ## when matching on time-fixed variables
  set.seed(134)
  out1 <- match_time(formula=vacc ~ mac,
                     data=d_single,
                     id=".id",
                     inclusion="inclusion")$data
  out1[, influenza := NULL]

  d_multi2 <- copy(d_multi)

  set.seed(134)
  out2 <- match_time.fit(id=".id",
                         time=".time",
                         d_treat=d_multi2$d_treat,
                         d_covars=d_multi2$d_covars,
                         match_vars="mac")$data

  expect_equal(out1, out2)

  ## when matching on time-dependent variables as well
  set.seed(134)
  out1 <- match_time(formula=vacc ~ mac + meds,
                     data=d_single,
                     id=".id",
                     inclusion="inclusion")$data
  out1[, influenza := NULL]

  d_multi2 <- copy(d_multi)

  set.seed(134)
  out2 <- match_time.fit(id=".id",
                         time=".time",
                         d_treat=d_multi2$d_treat,
                         d_covars=d_multi2$d_covars,
                         match_vars=c("mac", "meds"))$data

  expect_equal(out1, out2)

  ## when using id that is named "id"
  set.seed(134)
  setnames(d_single, old=".id", new="id")
  out3 <- match_time(formula=vacc ~ mac + meds,
                     data=d_single,
                     id="id",
                     inclusion="inclusion")$data
  out3[, influenza := NULL]
  setnames(out3, old="id", new=".id")
  setnames(d_single, old="id", new=".id")

  expect_equal(out3, out2)
})

test_that("using matchit", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac + meds,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    match_method="nearest")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1,], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(40, 40))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 40)
})

test_that("using Date input", {

  d_dates <- copy(d_single)
  d_dates[, start := as.Date(start, origin=as.Date("01.01.2000",
                                                   format="%d.%m.%Y"))]
  d_dates[, stop := as.Date(stop, origin=as.Date("01.01.2000",
                                                 format="%d.%m.%Y"))]

  set.seed(13534)
  out <- match_time(formula=vacc ~ meds,
                    data=d_dates,
                    id=".id",
                    inclusion="inclusion")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac not equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_true(tab[1,1] != tab[2,1])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1, ], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(49, 49))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 49)
})

test_that("match_method='none' with less controls than cases", {

  data("heart", package="survival")
  heart <- heart[, c("id", "start", "stop", "transplant", "age", "surgery")]

  # using matchit because age is continuous
  set.seed(12341432)
  m_obj <- suppressWarnings(
    match_time(transplant ~ age + surgery, data=heart, id="id",
               match_method="none", replace_over_t=FALSE)
  )

  expect_s3_class(m_obj, "match_time")
})

test_that("works with actual continuous data", {

  data("heart", package="survival")
  heart <- heart[, c("id", "start", "stop", "transplant", "age", "surgery")]

  # using matchit because age is continuous
  set.seed(12341432)
  m_obj <- suppressWarnings(
    match_time(transplant ~ age + surgery, data=heart, id="id",
               match_method="nearest", exact="surgery")
  )
  out <- match_data(m_obj)

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(51, 51))

  # surgery equally distributed in each level of .treat
  tab <- table(out$.treat, out$surgery)
  expect_true(tab[1,1] == tab[2,1])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$id))==2)
  out[, n_id := .N, by=id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(16, 16))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 34)

  # coerce to Date, do matching again and check if its the same output
  heart$start <- as.Date(heart$start, origin="1970-01-01")
  heart$stop <- as.Date(heart$stop, origin="1970-01-01")

  set.seed(12341432)
  m_obj <- suppressWarnings(
    match_time(transplant ~ age + surgery, data=heart, id="id",
               match_method="nearest")
  )
  out2 <- match_data(m_obj)
  out2[, .treat_time := as.numeric(.treat_time)]
  out2[, .next_treat_time := as.numeric(.next_treat_time)]
  out[, n_id := NULL]

  expect_equal(out2, out)
})

test_that("using replace_at_t=TRUE with MatchIt", {

  data("heart", package="survival")
  heart <- heart[, c("id", "start", "stop", "transplant", "age", "surgery")]

  # using matchit because age is continuous
  set.seed(12341432)
  m_obj <- suppressWarnings(
    match_time(transplant ~ age + surgery, data=heart, id="id",
               match_method="nearest", exact="surgery",
               replace_at_t=TRUE, ratio=1)
  )
  out <- match_data(m_obj)

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(53, 53))

  # surgery equally distributed in each level of .treat
  tab <- table(out$.treat, out$surgery)
  expect_true(tab[1,1] == tab[2,1])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id may occur more then twice
  expect_true(max(table(out$id))==3)
  out[, n_id := .N, by=id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(20, 18))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 37)
})

test_that("matching on fixed and time-dependent variable with psm method", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac + meds,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    method="psm",
                    match_method="nearest")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1,], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(40, 40))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 40)
})

test_that("using lp in method='psm'", {

  set.seed(134)
  out <- match_time(formula=vacc ~ mac + meds,
                    data=d_single,
                    id=".id",
                    inclusion="inclusion",
                    method="psm",
                    match_method="nearest",
                    ps_type="lp")$data

  # .treat equally distributed
  expect_equal(as.vector(table(out$.treat)), c(229, 229))

  # mac equally distributed in each level of .treat
  tab <- table(out$.treat, out$mac)
  expect_equal(tab[1, ], tab[2, ])

  # meds equally distributed in each level of .treat
  tab <- table(out$.treat, out$meds)
  expect_equal(tab[1,], tab[2, ])

  # pair id always occurs 2 times
  expect_true(all(table(out$.id_pair)==2))

  # .id_new is unique
  expect_true(length(unique(out$.id_new))==nrow(out))

  # .id only occurs once or twice, if twice then once as control and once
  # as a new case
  expect_true(max(table(out$.id))==2)
  out[, n_id := .N, by=.id]
  expect_equal(as.vector(table(out$.treat[out$n_id==2])), c(40, 40))

  # next treatment only possible for controls
  expect_equal(sum(!is.na(out$.next_treat_time[out$.treat])), 0)
  expect_equal(sum(!is.na(out$.next_treat_time[!out$.treat])), 40)
})
