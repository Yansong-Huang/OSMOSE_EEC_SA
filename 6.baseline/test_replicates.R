# script to test number of replicates needed

# simulation -----------------------------------------------------

library("osmose")
# 
configDir  = "osmose-eec"
configFile = file.path(configDir, "initial_config.csv")

# n_rep_values = c(5, 10, 15, 20, 30)
n_rep_values = c(35, 50)

for (n_rep in n_rep_values){
outputDir  = paste0("output_replicate_test_",n_rep)

jarFile   = file.path(configDir, "osmose_4.4.0-jar-with-dependencies.jar")

conf = read_osmose(input=configFile)

# replace replicates number
conf$simulation.nsimulation <- n_rep # test replicates number 

# NEW configuration file
name_new_config <- paste0("config_test_rep",n_rep,".csv")
write_osmose(conf, file = file.path(configDir, name_new_config),sep = ",")

new_config <- file.path(configDir, name_new_config)
run_osmose(input = new_config, output = outputDir, osmose = jarFile, version="4.4.0")# Reading outputs ---------------------------------------------------------

output_simu = read_osmose(path = file.path(outputDir), version = "4.4.0")

output_biomass  = get_var(output_simu, what = "biomass", expected = FALSE)
output_yield  = get_var(output_simu, what = "yield", expected = FALSE)

if (!dir.exists("biomass_output")) dir.create("biomass_output")
output_file_name <- file.path("biomass_output", paste0("biomass_n_rep", n_rep, ".rds"))
saveRDS(output_biomass, file=output_file_name)

if (!dir.exists("yield_output")) dir.create("yield_output")
output_file_name <- file.path("yield_output", paste0("yield_n_rep", n_rep, ".rds"))
saveRDS(output_yield, file=output_file_name)
}

