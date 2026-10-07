library(testthat)
library(DBI)

source(file.path("../../modular/db_connect.R"))
# Three Scernarios:
# 1. Con successfully with valid envir var.

test_that("db_connect returns a valid DBI connection obj", {
  skip_if(Sys.getenv("DB_HOST") == "", message = "No database environment configured")
  
  con <- db_connect()
  on.exit(close_connect(con)) # clean up if exit in mid
  
  # 1. Assert class
  expect_s4_class(con, "PqConnection")
  
  # 2. Assert active
  expect_true(dbIsValid(con))
})

# 2. Verify that close_connect() close the connection.
test_that("close_connect safely disconnected", {
  skip_if(Sys.getenv("DB_HOST") == "", message = "No database environment configured")
  
  con <- db_connect()
  expect_true(dbIsValid(con)) # Verify active before close
  result <- close_connect(con) # disconnect
  
  # 1. Assert function returned TRUE from closing
  expect_true(result) 
  
  # 2. Assert connection is no longer valid
  expect_false(dbIsValid(con))
})

# 3. Testing fallback/failure when environment variables or hosts are invalid.
test_that("db_connect fails gracefully on invalid host config", {
  # Temporarily override host variable to unreachable IP
  withr::with_envvar(new = c("DB_HOST" = "128.0.0.1"), {
    expect_error(
      db_connect(),
      regexp = ".*" # Expect connection error
    )
  })
})


