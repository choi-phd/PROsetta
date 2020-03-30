response_asq <- read.csv("inst/data-raw/dat_axmasq_v2.csv", stringsAsFactors = FALSE)
itemmap_asq  <- read.csv("inst/data-raw/imap_axmasq.csv"  , stringsAsFactors = FALSE)
anchor_asq   <- read.csv("inst/data-raw/anchor_axmasq.csv", stringsAsFactors = FALSE)
usethis::use_data(response_asq, overwrite = TRUE)
usethis::use_data(itemmap_asq , overwrite = TRUE)
usethis::use_data(anchor_asq  , overwrite = TRUE)

response_dep <- read.csv("inst/data-raw/dat_decesd_v2.csv", stringsAsFactors = FALSE)
itemmap_dep  <- read.csv("inst/data-raw/imap_decesd.csv"  , stringsAsFactors = FALSE)
anchor_dep   <- read.csv("inst/data-raw/anchor_decesd.csv", stringsAsFactors = FALSE)
usethis::use_data(response_dep, overwrite = TRUE)
usethis::use_data(itemmap_dep , overwrite = TRUE)
usethis::use_data(anchor_dep  , overwrite = TRUE)

data_asq <-
  loadData(
    response = response_asq,
    itemmap  = itemmap_asq,
    anchor   = anchor_asq
  )

data_dep <-
  loadData(
    response = response_dep,
    itemmap  = itemmap_dep,
    anchor   = anchor_dep
  )

usethis::use_data(data_asq, overwrite = TRUE)
usethis::use_data(data_dep, overwrite = TRUE)
