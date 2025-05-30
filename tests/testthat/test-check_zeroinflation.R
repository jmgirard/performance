test_that("check_zeroinflation", {
  skip_if_not_installed("glmmTMB")
  set.seed(123)
  data(Salamanders, package = "glmmTMB")
  m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)

  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 298,
        observed.zeros = 387L,
        ratio = 0.770025839793282,
        tolerance = 0.05
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, glmmTMB with and without zero-inflation component", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  set.seed(123)
  data(Salamanders, package = "glmmTMB")

  # no zero-inflation model
  m <- glmmTMB::glmmTMB(count ~ spp + mined, family = poisson, data = Salamanders)

  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 298,
        observed.zeros = 387L,
        ratio = 0.770025839793282,
        tolerance = 0.05
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )

  # zero-inflation model
  m <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    ziformula = ~ spp + mined,
    family = poisson,
    data = Salamanders
  )

  set.seed(123)
  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 387,
        observed.zeros = 387L,
        ratio = 1.00093023255814,
        tolerance = 0.1,
        p.value = 1
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, glmer.nb", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  set.seed(101)
  data(Salamanders, package = "glmmTMB")
  dd <- expand.grid(
    f1 = factor(1:3),
    f2 = LETTERS[1:2],
    g = 1:9,
    rep = 1:15,
    KEEP.OUT.ATTRS = FALSE
  )
  mu <- 5 * (-4 + with(dd, as.integer(f1) + 4 * as.numeric(f2)))
  dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
  dat2 <<- dd
  suppressMessages({
    m <- lme4::glmer.nb(y ~ f1 * f2 + (1 | g), data = dat2, verbose = FALSE)
  })

  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 153,
        observed.zeros = 155L,
        ratio = 0.987329032258065,
        tolerance = 0.1,
        p.value = 0.944
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, glmmTMB nbinom", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  skip_on_cran()

  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  set.seed(1234)
  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 389,
        observed.zeros = 387L,
        ratio = 1.00635658914729,
        tolerance = 0.1,
        p.value = 0.944
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, MASS::negbin", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("DHARMa")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- rnorm(500, mu, mu * 3)
  x <- ceiling(x)
  x <- pmax(x, 0)
  m <- MASS::glm.nb(x ~ mu)
  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 178,
        observed.zeros = 202L,
        ratio = 0.879643564356436,
        tolerance = 0.1,
        p.value = 0.008
      ),
      class = "check_zi"
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})


test_that("check_zeroinflation, genpois", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  model <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::genpois(),
    data = Salamanders
  )
  expect_equal(
    check_zeroinflation(model),
    structure(
      list(
        predicted.zeros = 386,
        observed.zeros = 387L,
        ratio = 0.997860465116279,
        tolerance = 0.1,
        p.value = 1
      ),
      class = "check_zi"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
