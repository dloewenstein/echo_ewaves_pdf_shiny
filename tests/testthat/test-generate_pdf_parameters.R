context("Generate PDF parameters")

test_that("generate_pdf_parameters in underdamped", {
    expect_equal(
        ewavesPDFshiny::generate_pdf_parameters(
            C = 15.9497,
            K = 221.5121,
            x0 = -0.114451,
            Epeak = 0.9,
            AT = 80,
            DT = 170
        ),
        list(
            Tau = 0.05517982,
            KFEI = 0.5680945,
            VTI = 0.130038,
            peak_driving_force = -25.35228,
            peak_resistive_force = 14.35473,
            damping_index = -631.6555,
            filling_energy = 1.450797
        ),
        tolerance = 1e-4
    )
}
)

