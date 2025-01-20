
bats.asia.nonpublish <- read_excel('F:/Working/2018/PhD_research/SDM output R/20_4_23/vnbats_unpublish_2022.xlsx')

#change synonym
bats.asia.nonpublish$species[bats.asia.nonpublish$species=='Arielulus aureocollaris'] <- "Thainycteris aureocollaris"
bats.asia.nonpublish$species[bats.asia.nonpublish$species=='Coelops frithi'] <- "Coelops frithii"
bats.asia.nonpublish$species[bats.asia.nonpublish$species=='heathii'] <- "Scotophilus heathii"
bats.asia.nonpublish$species[bats.asia.nonpublish$species=='microglobosus'] <- "Rhinolophus microglobosus"
bats.asia.nonpublish$species[bats.asia.nonpublish$species=='pearsonii'] <- "Rhinolophus pearsonii"

bats.asia.nonpublish$source[bats.asia.nonpublish$source=='Dr. Thong'] <- "Dr.Thong"
bats.asia.nonpublish$source[bats.asia.nonpublish$source=='Dr. Tu'] <- "Dr.Tu"
bats.asia.nonpublish$source[bats.asia.nonpublish$source=='Dr. Thong'] <- "Dr.Thong"
bats.asia.nonpublish <- bats.asia.nonpublish%>%select(species,updated_name,decimalLongitude,decimalLatitude,ID,elevation,date,
                                                      year,locality,family,genus,source,country)
write.xlsx(bats.asia.nonpublish, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_literature_coauthors_2022.xlsx")

