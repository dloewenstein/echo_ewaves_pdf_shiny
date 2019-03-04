context("Viscoelastic energyloss (c), stiffness (k) and Load (x0)")
devtools::load_all()

test_that("generate_c_k_x0 in underdamped", {
    expect_equal(
        generate_c_k_x0(50, 200, 1.1),
        list(
            K = 457.0696,
            C = 34.5923,
            x0 = -0.1222
        ),
        tolerance = 1e-4
    ) 
}
)

test_that("generate_c_k_x0 in overdamped", {
    expect_equal(
        generate_c_k_x0(60, 350, 0.8),
        list(
            K = 202.5434,
            C = 43.5478,
            x0 = -0.2076
        ),
        tolerance = 1e-4
    )
}
)

