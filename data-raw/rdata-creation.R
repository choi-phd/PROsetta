anchor_asq = read.csv("data-raw\\anchor_axmasq.csv")
response_asq = read.csv("data-raw\\dat_axmasq_v2.csv")
itemmap_asq = read.csv("data-raw\\imap_axmasq.csv")

save(anchor_asq, file = "data\\anchor_asq.RData")
save(response_asq, file = "data\\response_asq.RData")
save(itemmap_asq, file = "data\\itemmap_asq.RData")
