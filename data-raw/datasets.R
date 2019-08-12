response_asq <- read.csv("data-raw/dat_axmasq_v2.csv")
anchor_asq   <- read.csv("data-raw/anchor_axmasq.csv")
itemmap_asq  <- read.csv("data-raw/imap_axmasq.csv")

cfg_asq <- createConfig(
  response_file = "data-raw/dat_axmasq_v2.csv",
  anchor_file   = "data-raw/anchor_axmasq.csv",
  itemmap_file  = "data-raw/imap_axmasq.csv",
  guess_id = TRUE
)

usethis::use_data(response_asq, overwrite = TRUE)
usethis::use_data(anchor_asq, overwrite = TRUE)
usethis::use_data(itemmap_asq, overwrite = TRUE)
usethis::use_data(cfg_asq, overwrite = TRUE)
