context("Helper functions")


test_that("Duration above max", {
    expect_true(check_duration(501))
})

test_that("Duration under min", {
    expect_true(check_duration(9))
})

test_that("Velocity above max", {
    expect_true(check_velocity(5.1))
})

test_that("Velocity under min", {
    expect_true(check_velocity(0.09))
})

test_that("Convert acceleration to time", {
    expect_equal(
        convert_to_time(2.1, 30),
        70)
})

test_that("Convert time to acceleration", {
    expect_equal(
        convert_to_acceleration(2.1, 70),
        3000
    )
})
