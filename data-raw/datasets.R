response_asq <- read.csv("data-raw/dat_axmasq_v2.csv")
anchor_asq   <- read.csv("data-raw/anchor_axmasq.csv")
itemmap_asq  <- read.csv("data-raw/imap_axmasq.csv")

usethis::use_data(response_asq, overwrite = TRUE)
usethis::use_data(anchor_asq, overwrite = TRUE)
usethis::use_data(itemmap_asq, overwrite = TRUE)
