library(data.table);library(magrittr);library(jstable);library(survey)
#setwd("~/ShinyApps/SMC_SAIHST/danbee/NHANES")
setwd("/home/heejooko/ShinyApps/saihst-nhanes")


out <- readRDS("NHANES_AYA_final.RDS")

out[, n_fam := as.integer((n_fam >= 2) + (n_fam >= 4) + ( n_fam>= 6))]
out[, r_pregn := ifelse(is.na(r_preg == 0), 0, r_pregn)][, r_pregn := as.integer((r_pregn >=1) + (r_pregn >=2) + (r_pregn >=5) + (r_pregn >=11))]
out[, job_type := ifelse(job_stat == "2", "0", as.character(job_type))]


varlist <- list(Base = c("cancer", "AYA", "p_age", "country", "racecat", "p_sex", "BMI", "p_edu", "p_marri", "job_stat", "job_type",  "house_inc",
                         "job_hr", "g_health", "n_fam", "alc", "smk"),
                Disease = c("art_d", "ast_d", "hbp_d", "chol_d", "diab_d", "stk_d", "ang_d", "mi_d", "thy_d"),
                Physical = c("pf_lim", "phy_v", "phy_vn", "phy_m", "phy_mn"), 
                Reproduce = c("r_pregn", "r_preg", "r_last"),
                CancerType = c("stomach_c", "liver_c", "colon_c", "breast_c", "cervix_c", "lung_c", "thy_c", "other_c"),
                Survey = c("p_id", "strata", "persweight"),
                CancerAge = c("stomach_a","liver_a","colon_a","breast_a","cervix_a","lung_a","thy_a","other_a","cancer_a"),
                Time = c("stomach_t","liver_t","colon_t","breast_t","cervix_t","lung_t","thy_t","other_t","cancer_t"))


out[,cancer_a:=pmin(stomach_a,liver_a,colon_a,breast_a,cervix_a,lung_a,thy_a,other_a,na.rm=T)]
for(v in varlist$CancerAge){
  vt<-gsub("_a","_t",v)
  out[[vt]]<-out[["p_age"]]-out[[v]]
}

#for (v in c("phy_vn", "phy_mn")){
#  out[[v]] <- ifelse(is.na(out[[v]]), "0", as.character(out[[v]]))
#}

factor_vars <- c("cancer", "AYA", "country", "racecat", "p_sex", "p_edu", "p_marri", "job_stat", "job_type", "r_pregn", "r_preg", "house_inc", "g_health", "n_fam",
                 varlist$Disease, varlist$Physical, varlist$CancerType, "alc", "smk","BMI")
factor_vars <- factor_vars[factor_vars %in% names(out)]
conti_vars <- c("p_age", "job_hr","stomach_a","liver_a","colon_a","breast_a","cervix_a","lung_a","thy_a")
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

#out.svy <- svydesign(data = out, id =~p_id, strata = ~strata, weights = ~persweight, nest = T)

out$pf_lim <- relevel(out$pf_lim, ref = "2")
out$job_stat <- relevel(out$pf_lim, ref = "2")

out.label <- mk.lev(out)

out.label[variable == "cancer", `:=`(var_label = "Cancer", val_label = c("No", "Yes"))]
out.label[variable == "AYA", `:=`(var_label = "AYA", val_label = c("No", "Yes"))]
out.label[variable == "p_age", `:=`(var_label = "Age")]
out.label[variable == "p_sex", `:=`(var_label = "Sex", val_label = c("Male", "Female"))]
out.label[variable == "racecat", `:=`(var_label = "Race")]
out.label[variable == "stomach_t", `:=` (var_label = "Time after Stomach cancer diagnosis")]
out.label[variable == "liver_t", `:=` (var_label = "Time after Liver cancer diagnosis")]
out.label[variable == "colon_t", `:=` (var_label = "Time after Colon cancer diagnosis")]
out.label[variable == "breast_t", `:=` (var_label = "Time after Breast cancer diagnosis")]
out.label[variable == "cervix_t", `:=` (var_label = "Time after Cervix cancer diagnosis")]
out.label[variable == "lung_t", `:=` (var_label = "Time after Lung cancer diagnosis")]
out.label[variable == "thy_t", `:=` (var_label = "Time after Thyroid cancer diagnosis")]
out.label[variable == "other_t", `:=` (var_label = "Time after other cancer diagnosis")]
out.label[variable == "cancer_t", `:=` (var_label = "Time after any cancer diagnosis")]
out.label[variable == "stomach_a", `:=` (var_label = "Age at Stomach cancer diagnosis")]
out.label[variable == "liver_a", `:=` (var_label = "Age at Liver cancer diagnosis")]
out.label[variable == "colon_a", `:=` (var_label = "Age at Colon cancer diagnosis")]
out.label[variable == "breast_a", `:=` (var_label = "Age at Breast cancer diagnosis")]
out.label[variable == "cervix_a", `:=` (var_label = "Age at Cervix cancer diagnosis")]
out.label[variable == "lung_a", `:=` (var_label = "Age at Lung cancer diagnosis")]
out.label[variable == "thy_a", `:=` (var_label = "Age at Thyroid cancer diagnosis")]
out.label[variable == "other_a", `:=` (var_label = "Age at other cancer diagnosis")]
out.label[variable == "cancer_a", `:=` (var_label = "Age at any cancer diagnosis")]
out.label[variable == "p_edu", `:=`(var_label = "Education", val_label = c("Less than high school graduate", "High school graduate/GED or equivalent", "College/University graduate or above"))]
out.label[variable == "p_marri", `:=`(var_label = "Marital status", val_label = c("Single", "Married", "Divorced", "Widowed", "Separated", "Living with partner"))]
out.label[variable == "g_health", `:=`(var_label = "General health", val_label = c("Excellent", "Very good", "Good", "Fair", "Poor"))]
out.label[variable == "pf_lim", `:=`(var_label = "Physical, mental, emotional limitation", val_label = c("No", "Yes"))]
out.label[variable == "phy_v", `:=`(var_label = "Vigorous physical activity", val_label = c("No", "Yes"))]
out.label[variable == "phy_vn", `:=`(var_label = "Vigorous physical activity(day)", val_label = c("1-3", "4-6", "7"))]
out.label[variable == "phy_m", `:=`(var_label = "Moderate physical activity", val_label = c("No", "Yes"))]
out.label[variable == "phy_mn", `:=`(var_label = "Moderate physical activity(day)", val_label = c("1-3", "4-6", "7"))]
out.label[variable == "n_fam", `:=`(var_label = "Number of people in the household", val_label = c("1", "2-3", "4-5", "6 more"))]
out.label[variable == "house_inc", `:=`(var_label = "Income", val_label = c("Less than $20,000", "$20,000 to $54,999", "$55,000 to $74,999", "More than $75,000"))]
out.label[variable == "r_preg", `:=`(var_label = "Pregnant", val_label = c("No", "Yes"))]
out.label[variable == "r_pregn", `:=`(var_label = "Number of pregnancies", val_label = c("0", "1", "2-4", "5-10", "11 more"))]
out.label[variable == "job_stat", `:=`(var_label = "Job status", val_label = c("Unemployed", "Employed"))]
out.label[variable == "job_type", `:=`(var_label = "Job type", val_label = c("Unemployed", "Wage worker", "Self-employed", "Family business without pay"))]
out.label[variable == "job_hr", `:=`(var_label = "Working hours(/week)")]
out.label[variable == "stomach_c", `:=`(var_label = "Stomach cancer", val_label = c("No", "Yes"))]
out.label[variable == "liver_c", `:=`(var_label = "Liver cancer", val_label = c("No", "Yes"))]
out.label[variable == "colon_c", `:=`(var_label = "Colon cancer", val_label = c("No", "Yes"))]
out.label[variable == "breast_c", `:=`(var_label = "Breast cancer", val_label = c("No", "Yes"))]
out.label[variable == "cervix_c", `:=`(var_label = "Cervix cancer", val_label = c("No", "Yes"))]
out.label[variable == "lung_c", `:=`(var_label = "Lung cancer", val_label = c("No", "Yes"))]
out.label[variable == "thy_c", `:=`(var_label = "Thyroid cancer", val_label = c("No", "Yes"))]
out.label[variable == "other_c", `:=`(var_label = "Other cancer", val_label = c("No", "Yes"))]
out.label[variable == "art_d", `:=`(var_label = "Arthritis Hx", val_label = c("No", "Yes"))]
out.label[variable == "ast_d", `:=`(var_label = "Asthma Hx", val_label = c("No", "Yes"))]
out.label[variable == "hbp_d", `:=`(var_label = "HTN Hx", val_label = c("No", "Yes"))]
out.label[variable == "chol_d", `:=`(var_label = "Dyslipidemia Hx", val_label = c("No", "Yes"))]
out.label[variable == "diab_d", `:=`(var_label = "DM Hx", val_label = c("No", "Yes"))]
out.label[variable == "stk_d", `:=`(var_label = "Stroke Hx", val_label = c("No", "Yes"))]
out.label[variable == "ang_d", `:=`(var_label = "Angina Hx", val_label = c("No", "Yes"))]
out.label[variable == "mi_d", `:=`(var_label = "MI Hx", val_label = c("No", "Yes"))]
out.label[variable == "thy_d", `:=`(var_label = "Thyroid Hx", val_label = c("No", "Yes"))]
out.label[variable == "alc", `:=`(var_label = "Alcohol", val_label = c("Never", "Former", "Current"))]
out.label[variable == "smk", `:=`(var_label = "Smoke", val_label = c("Never", "Former", "Current"))]
out.label[variable == "BMI", `:=`(var_label = "BMI", val_label = c("Underweight", "Normalr", "Overweight","Obese"))]

