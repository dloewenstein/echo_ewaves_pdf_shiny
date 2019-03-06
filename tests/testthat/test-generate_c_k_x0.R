context("Viscoelastic energyloss (c), stiffness (k) and Load (x0)")

test_that("generate_c_k_x0 in underdamped", {
    expect_equal(
        ewavesPDFshiny::generate_c_k_x0(AT = 80, DT = 170, Epeak = 0.9),
        list(
            K = 221.5121,
            C = 15.9497,
            x0 = -0.114451
        ),
        tolerance = 1e-4
    )
}
)

test_that("generate_c_k_x0 in overdamped", {
    expect_equal(
        ewavesPDFshiny::generate_c_k_x0(AT = 60, DT = 350, Epeak = 0.8),
        list(
            K = 202.5434,
            C = 43.5478,
            x0 = -0.2076
        ),
        tolerance = 1e-4
    )
}
)

