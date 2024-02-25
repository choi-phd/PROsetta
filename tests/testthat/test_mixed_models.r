library(TestDesign)
library(PROsetta)

rmse <- function (x1, x2) {
  v <- sqrt(mean((x2 - x1) ** 2))
  return(v)
}

idx_anchor_items <- 1:10
idx_new_items    <- 11:30
n_items <- length(c(idx_anchor_items, idx_new_items))

n_participants <- 20000

test_cases <- c("all_GR", "all_GPC", "mixed")

for (test_case in test_cases) {

  test_that(
    sprintf("anchoring is done correctly with different item models: %s", test_case),
  {

    skip_on_ci()
    skip_on_cran()

    item_parameters <- as.data.frame(matrix(NA, n_items, 0))

    set.seed(1)

    for (i in 1:n_items) {

      m <- NULL
      if (test_case == "all_GR")  m <- "GR"
      if (test_case == "all_GPC") m <- "GPC"
      if (test_case == "mixed")   m <- sample(c("GPC", "GR"), 1)

      if (m == "GR") {
        item_parameters$ID[i] <- sprintf("ITEM%02i", i)
        item_parameters$MODEL[i] <- "GR"
        item_parameters$PAR1[i] <- runif(1, 1, 2)
        item_parameters$PAR2[i] <- runif(1, 0, 1)
        item_parameters$PAR3[i] <- runif(1, 1, 2)
      }
      if (m == "GPC") {
        item_parameters$ID[i] <- sprintf("ITEM%02i", i)
        item_parameters$MODEL[i] <- "GPC"
        item_parameters$PAR1[i] <- runif(1, 1, 2)
        item_parameters$PAR2[i] <- runif(1, -2, 2)
        item_parameters$PAR3[i] <- runif(1, -2, 2)
      }

      if (i %in% idx_anchor_items) {
        item_parameters$ID[i] <- sprintf("%s_ANCHORED", item_parameters$ID[i])
      }

    }

    item_pool <- loadItemPool(item_parameters)

    # make response data

    set.seed(1)
    true_thetas <- rnorm(n_participants, 2, 1)

    set.seed(1)
    response <- simResp(item_pool, true_thetas)
    response <- as.data.frame(response)
    names(response) <- item_parameters$ID
    response <- cbind(person_id = sprintf("person_%s", 1:nrow(response)), response)

    # make itemmap
    itemmap <- data.frame(
      instrument = NA,
      item_id = item_parameters$ID,
      item_model = item_parameters$MODEL,
      ncat = 3
    )
    itemmap$instrument[idx_anchor_items] <- 1
    itemmap$instrument[idx_new_items]    <- 2

    # make anchor data
    anchor_parameters <- item_parameters[idx_anchor_items, ]
    names(anchor_parameters) <- c("item_id", "item_model", "a", "cb1", "cb2")

    # now package as a data object
    d <- loadData(
      response, itemmap, anchor_parameters
    )

    # test each linking method

    for (
      linking_method in
      c("FIXEDPAR", "MM", "MS", "HB", "SL", "CP", "CPLA")
      # don't check CPFIXEDDIM because the metric is slightly off
      # - the discrepancy comes from estimation error in mean(theta) and var(theta)
    ) {

      suppressWarnings(
        o <- runLinking(
          d, method = linking_method,
          technical = list(keep_vcov_PD = FALSE)
        )
      )

      # collapse dimensions to one for CP methods, for comparison purposes
      if (linking_method %in% c("CP", "CPLA")) {
        o$ipar_linked$a <- o$ipar_linked$a1 + o$ipar_linked$a2
        o$ipar_linked$a1 <- NULL
        o$ipar_linked$a2 <- NULL
        o$ipar_linked <- PROsetta:::convertADtoAB(o$ipar_linked, d@item_id, d@model_id)
      }

      expect_lt(
        rmse(item_parameters$PAR1[idx_anchor_items], o$ipar_linked$a[idx_anchor_items]),
        0.05
      )
      expect_lt(
        rmse(item_parameters$PAR2[idx_anchor_items], o$ipar_linked$b1[idx_anchor_items]),
        0.05
      )
      expect_lt(
        rmse(item_parameters$PAR3[idx_anchor_items], o$ipar_linked$b2[idx_anchor_items]),
        0.05
      )

    }

  })

}
