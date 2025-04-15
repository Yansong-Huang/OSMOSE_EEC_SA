### Pour compiler l'ensemble des simu dans un fichier list output
# Criscely Lujan, Yansong Huang
# 15/04/2025

rm(list = ls())

output_dir = "simulation_results_test"
doe = readRDS(file = "2.get-doe/doe/split_3_0413.rds")

rds_files <- list.files(path = output_dir, pattern = "\\.rds$", full.names = TRUE)
out = lapply(rds_files, FUN = readRDS) 
class(out) = c("doe_output", class(out))
attr(out, "doe") = doe

saveRDS(out,file.path(output_dir,"aggregated_test.rds"))
