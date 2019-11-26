test_that("option default port", {
  expect_equal(get_proxy_port(), 9000L)
})

test_that("option change port", {
  set_proxy_port(9191L)
  expect_equal(get_proxy_port(), 9191L)
})

test_that("option default null key", {
  expect_null(get_app_key())
})

test_that("option set some key", {
  set_app_key('MY_APP_KEY')
  expect_equal(get_app_key(), 'MY_APP_KEY')
})
