# SPDX-License-Identifier: GPL-2.0

# Load the MRA function
source("MRA.R")

library(metafor)

# Parameters
num_markers = 10000
num_studies = 10

# Initialize an empty data frame to store 10 Studies simulated data
final_data = data.frame(matrix(nrow = num_markers, ncol = 22))
names(final_data) = c(paste0("beta_study_", 1:num_studies), 
                       paste0("se_study_", 1:num_studies), 
                       "meta_beta", "meta_se")

# Simulate data and perform meta-analysis
for (i in 1:num_markers) {
  # Simulate betas and ses
  betas = rnorm(num_studies, mean=0, sd=1)
  ses = runif(num_studies, min=0.1, max=0.5)
  
  # Perform meta-analysis
  result = rma(yi = betas, sei = ses, method = "FE")
  
  # Store simulated betas, ses, and meta-analysis results in the data frame
  final_data[i, 1:num_studies] = betas
  final_data[i, (num_studies+1):(2*num_studies)] = ses
  final_data[i, "meta_beta"] = coef(result)
  final_data[i, "meta_se"] = sqrt(vcov(result))
}

# Leave out one study and perform meta-analysis
sub_data = final_data[, c(1:9, 11:19, 21, 22)]
sub_data$meta_beta = NA
sub_data$meta_se = NA

for (i in 1:num_markers) {
  
  betas = as.numeric(sub_data[i,1:9])
  ses = as.numeric(sub_data[i,10:18])
  # Perform meta-analysis
  result = rma(yi = betas, sei = ses, method = "FE")
  
  sub_data[i, "meta_beta"] = coef(result)
  sub_data[i, "meta_se"] = sqrt(vcov(result))
}

# Test the MRA on simulated data
test = MRA(final_data$meta_beta,final_data$meta_se,final_data$beta_study_10,final_data$se_study_10)

# Visualize and test correlation
plot(test$beta_red,sub_data$meta_beta)
plot(test$SE_red,sub_data$meta_se)

cor.test(test$beta_red,sub_data$meta_beta)
