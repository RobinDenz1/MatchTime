
set.seed(12344)

data("heart", package="survival")
heart$fake <- rnorm(n=nrow(heart))
heart2 <- heart[, c("id", "start", "stop", "transplant", "surgery")]

set.seed(1234)
m_obj1 <- match_time(transplant ~ surgery, data=heart2, id="id",
                     match_method="fast_exact")

set.seed(1234)
m_obj2 <- match_time(transplant ~ surgery, data=heart, id="id",
                     match_method="fast_exact")
m_obj2$data[, event := NULL]
m_obj2$data[, year := NULL]
data.table::setindexv(m_obj2$data, NULL)

test_that("same as just having the variable in data", {

  m_obj3 <- add_from_start_stop(m_obj1, data=heart, variable="fake")
  m_obj3 <- add_from_start_stop(m_obj3, data=heart, variable="age")

  expect_true(all.equal(m_obj2$data, m_obj3$data, ignore.col.order=TRUE))
})

test_that("works with other start / stop names", {

  heart$beginning <- heart$start
  heart$ending <- heart$stop

  m_obj3 <- add_from_start_stop(m_obj1, data=heart, variable="fake",
                                start="beginning", stop="ending")
  m_obj3 <- add_from_start_stop(m_obj3, data=heart, variable="age",
                                start="beginning", stop="ending")

  expect_true(all.equal(m_obj2$data, m_obj3$data, ignore.col.order=TRUE))
})

test_that("default argument works", {

  heart$fake[heart$id %in% c(1, 2, 3, 4, 5)] <- NA

  # just using NA
  m_obj3 <- add_from_start_stop(m_obj1, data=heart, variable="fake",
                                default=NA)

  expect_true(all(is.na(m_obj3$data[id %in% c(1, 2, 3, 4, 5)]$fake)))
  expect_true(all(!is.na(m_obj3$data[!id %in% c(1, 2, 3, 4, 5)]$fake)))

  # using something else
  m_obj3 <- add_from_start_stop(m_obj1, data=heart, variable="fake",
                                default=1000)

  expect_true(all(m_obj3$data[id %in% c(1, 2, 3, 4, 5)]$fake==1000))
  expect_true(all(m_obj3$data[!id %in% c(1, 2, 3, 4, 5)]$fake!=1000))
})
