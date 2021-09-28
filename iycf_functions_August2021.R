### made by Saaeed
### last edit August 26/08/2021
library(tidyverse)
library(readxl)

extract_iycf_v2021 <- function(df,
                               sex = NULL, #sex of the child
                               date_birth = NULL, # date of birth
                               age_days = NULL, #age in days
                               age_months, #age in months
                               iycf_1 = NULL, # ever breastfed (y.n)
                               iycf_2 = NULL, # how long after birth breastfed
                               iycf_3 = NULL, # given anything to eat/drink in first 2 days after delivery
                               iycf_4 = NULL, # breastfed yesterday during the day or night (y/n)
                               iycf_5 = NULL, # drink anything from bottle with a nipple (y/n)
                               iycf_6a = NULL, # plain water
                               iycf_6b = NULL, # infant formula (y/n)
                               iycf_6b_num = NULL, # infant formula (number)
                               iycf_6c = NULL, # milk from animals, fresh tinned powder (y/n)
                               iycf_6c_num = NULL, # milk form animals, fresh, tinned, pwder (number)
                               iycf_6c_swt = NULL, # milk was sweetened (y/n)
                               iycf_6d = NULL, # yoghurt drinks (y/n)
                               iycf_6d_num = NULL, # yoghurt drinks (number)
                               iycf_6d_swt = NULL, # yoghurt drink was sweetened (y/n)
                               iycf_6e = NULL, # chocolate flavoured drinks, including from syrup / powders (y/n)
                               iycf_6f = NULL, # Fruit juice or fruit-flavoured drinks including those made from syrups or powders? (y/n)
                               iycf_6g = NULL, # sodas, malt drinks, sports and energy drinks (y/n)
                               iycf_6h = NULL, # tea, coffee, herbal drinks (y/n)
                               iycf_6h_swt = NULL, # tea coffee herbal drinks were sweetened (y/n) 
                               iycf_6i = NULL, # clear broth / soup (y/n)
                               iycf_6j = NULL, # other liquids (y/n)
                               iycf_6j_swt = NULL, # other drinks were sweetened (y/n)
                               iycf_7a = NULL, # yoghurt (NOT yoghurt drinks) (y/n)
                               iycf_7a_num = NULL, # yoghurt (NOT yoghurt drinks) (number)
                               iycf_7b = NULL, # porridge, bread, rice, nooodles (y/n)
                               iycf_7c = NULL, # vitamin a rich vegetables (pumpkin, carrots, sweet red peppers, squash or yellow/orange sweet potatoes) (y/n)
                               iycf_7d = NULL, # white starches (plaintains, white potatoes, white yams, manioc, cassava) (y/n) 
                               iycf_7e = NULL, # dark green leafy vegetables (y/n)
                               iycf_7f = NULL, # other vegetables (y/n)
                               iycf_7g = NULL, # vitamin a rich fruits (ripe mangoes, ripe papayas) (y/n)
                               iycf_7h = NULL, # other fruits (y/n)
                               iycf_7i = NULL, # organ meats (liver ,kidney, heart) (y/n)
                               iycf_7j = NULL, # processed meats (sausages, hot dogs, ham, bacon, salami, canned meat) (y/n)
                               iycf_7k = NULL, # any other meats (beef, chicken, pork, goat, chicken, duck) (y/n)
                               iycf_7l = NULL, # eggs (y/n)
                               iycf_7m = NULL, # fish (fresh or dried fish or shellfish) (y/n)
                               iycf_7n = NULL, # legumes (beans, peas, lentils, seeds, chickpeas) (y/n)
                               iycf_7o = NULL, # cheeses (hard or soft cheeses) (y/n)
                               iycf_7p = NULL, # sweets (chocolates, candies, pastries, cakes) (y.n)
                               iycf_7q = NULL, # fried or empty carbs (chips, crisps, french fries, fried dough, instant noodles) (y/n)
                               iycf_7r = NULL, # Any other solid, semi-solid, or soft foods
                               iycf_7s = NULL, # did child eat solid/semi-solid foods (y/n) for list based questionnaires
                               iycf_8 = NULL # times child ate solid/semi-solid foods (number)
) {

  
  df <- df %>% 
    rename(sex = {{sex}},
           date_of_birth = {{date_birth}},
           age_days = {{age_days}},
           age_months = {{age_months}}, # age in months
           iycf_1 = {{iycf_1}}, # ever breastfed (y.n)
           iycf_2 = {{iycf_2}}, # how long after birth breastfed
           iycf_3 = {{iycf_3}}, # given anything to eat/drink in first 2 days after delivery
           iycf_4 = {{iycf_4}}, # breastfed yesterday during the day or night (y/n)
           iycf_5 = {{iycf_5}}, # drink anything from bottle with a nipple (y/n)
           iycf_6a = {{iycf_6a}}, # plain water
           iycf_6b = {{iycf_6b}}, # infant formula (y/n)
           iycf_6b_num = {{iycf_6b_num}}, # infant formula (number)
           iycf_6c = {{iycf_6c}}, # milk from animals, fresh tinned powder (y/n)
           iycf_6c_num = {{iycf_6c_num}}, # milk form animals, fresh, tinned, pwder (number)
           iycf_6c_swt = {{iycf_6c_swt}}, # milk was sweetened (y/n)
           iycf_6d = {{iycf_6d}}, # yoghurt drinks (y/n)
           iycf_6d_num = {{iycf_6d_num}}, # yoghurt drinks (number)
           iycf_6d_swt = {{iycf_6d_swt}}, # yoghurt drink was sweetened (y/n)
           iycf_6e = {{iycf_6e}}, # chocolate flavoured drinks, including from syrup / powders (y/n)
           iycf_6f = {{iycf_6f}}, # Fruit juice or fruit-flavoured drinks including those made from syrups or powders? (y/n)
           iycf_6g = {{iycf_6g}}, # sodas, malt drinks, sports and energy drinks (y/n)
           iycf_6h = {{iycf_6h}}, # tea, coffee, herbal drinks (y/n)
           iycf_6h_swt = {{iycf_6h_swt}}, # tea coffee herbal drinks were sweetened (y/n) 
           iycf_6i = {{iycf_6i}}, # clear broth / soup (y/n)
           iycf_6j = {{iycf_6j}}, # other liquids (y/n)
           iyf_6j_swt = {{iycf_6j_swt}}, # other drinks were sweetened (y/n)
           iycf_7a = {{iycf_7a}}, # yoghurt (NOT yoghurt drinks) (y/n)
           iycf_7a_num = {{iycf_7a_num}}, # yoghurt (NOT yoghurt drinks) (number)
           iycf_7b = {{iycf_7b}}, # porridge, bread, rice, nooodles (y/n)
           iycf_7c = {{iycf_7c}}, # vitamin a rich vegetables (pumpkin, carrots, sweet red peppers, squash or yellow/orange sweet potatoes) (y/n)
           iycf_7d = {{iycf_7d}}, # white starches (plaintains, white potatoes, white yams, manioc, cassava) (y/n) 
           iycf_7e = {{iycf_7e}}, # dark green leafy vegetables (y/n)
           iycf_7f = {{iycf_7f}}, # other vegetables (y/n)
           iycf_7g = {{iycf_7g}}, # vitamin a rich fruits (ripe mangoes, ripe papayas) (y/n)
           iycf_7h = {{iycf_7h}}, # other fruits (y/n)
           iycf_7i = {{iycf_7i}}, # organ meats (liver ,kidney, heart) (y/n)
           iycf_7j = {{iycf_7j}}, # processed meats (sausages, hot dogs, ham, bacon, salami, canned meat) (y/n)
           iycf_7k = {{iycf_7k}}, # any other meats (beef, chicken, pork, goat, chicken, duck) (y/n)
           iycf_7l = {{iycf_7l}}, # eggs (y/n)
           iycf_7m = {{iycf_7m}}, # fish (fresh or dried fish or shellfish) (y/n)
           iycf_7n = {{iycf_7n}}, # legumes (beans, peas, lentils, seeds, chickpeas) (y/n)
           iycf_7o = {{iycf_7o}}, # cheeses (hard or soft cheeses) (y/n)
           iycf_7p = {{iycf_7p}}, # sweets (chocolates, candies, pastries, cakes) (y.n)
           iycf_7q = {{iycf_7q}}, # fried or empty carbs (chips, crisps, french fries, fried dough, instant noodles) (y/n)
           iycf_7r = {{iycf_7r}}, # Any other solid, semi-solid, or soft foods
           iycf_7s = {{iycf_7s}}, # did child eat solid/semi-solid foods (y/n) for list based questionnaires
           iycf_8 = {{iycf_8}} # times child ate solid/semi-solid foods (number))
    )
  
  # calculating any tertiary yn/number vars that may not have been included
  
  if(is.null(iycf_6b) & !is.null(iycf_6b_num)) {df <- df %>% mutate(iycf_6b = ifelse(is.na(iycf_6b_num), NA, ifelse(iycf_6b_num == 0,"n", "y")))}
  if(is.null(iycf_6c) & !is.null(iycf_6c_num)) {df <- df %>% mutate(iycf_6c = ifelse(is.na(iycf_6c_num), NA, ifelse(iycf_6c_num == 0,"n", "y")))}
  if(is.null(iycf_6d) & !is.null(iycf_6d_num)) {df <- df %>% mutate(iycf_6d = ifelse(is.na(iycf_6d_num), NA, ifelse(iycf_6d_num == 0,"n", "y")))}
  if(is.null(iycf_7a) & !is.null(iycf_7a_num)) {df <- df %>% mutate(iycf_7a = ifelse(is.na(iycf_7a_num), NA, ifelse(iycf_7a_num == 0,"n", "y")))}

  
  # checks for which indicators can be calculated
  print("The following standard IYCF indicators can, or cannot, be calculated with the given inputs. Only checking based on columns provided, has not checked column values yet.")
  if(!is.null(iycf_1) & !is.null(age_months)) {print("IYCF Indicator 1: Ever Breastfed; YES")} else {print("IYCF Indicator 1: Ever Breastfed; NO")}
  if(!is.null(iycf_2) & !is.null(age_months)) {print("IYCF Indicator 2: Early Initiation of Breastfeeding; YES")} else {print("IYCF Indicator 2: Early Initiation of Breastfeeding; NO")}
  if(!is.null(iycf_3) & !is.null(age_months)) {print("IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth; YES")} else {print("IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth; NO")}
  
  ebf_vars <- c("iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  
  if(!is.null(age_months) & length(setdiff(ebf_vars, colnames(df)))== 0) {print("IYCF Indicator 4: Exclusive Breastfeeding; YES")} else {print("IYCF Indicator 2: Exclusive Breastfeeding; NO")}

  if(!is.null(age_months) & !is.null(iycf_4) & !is.null(iycf_6b) & !is.null(iycf_6c)) {print("IYCF Indicator 5: Mixed Milk Feeding (MIxMF); YES")} else {print("IYCF Indicator 5: Mixed Milk Feeding (MIxMF); NO")}
  if(!is.null(age_months) & !is.null(iycf_4)) {print("IYCF Indicator 6: Continued Breastfeeding 12-23 months; YES")} else {print("IYCF Indicator 6: Continued Breastfeeding 12-23 months; NO")}
  
  food_vars <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  ###YS correcting parenthesis position in test
  if(!is.null(age_months) & length(setdiff(food_vars, colnames(df))) == 0) {print("IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods; YES")} else {print("IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods; NO")}
  
  mdd_vars <- c("iycf_4", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o")
  if(!is.null(age_months) & length(setdiff(mdd_vars, colnames(df)))==0) {print("IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD); YES")} else {print("IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD); NO")}
  
  mmf_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_8")
  if(!is.null(age_months) & length(setdiff(mmf_vars, colnames(df)))==0) {print("IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF); YES")} else {print("IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF); NO")}
  
  mmff_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_7a_num")
  if(!is.null(age_months) & length(setdiff(mmff_vars, colnames(df)))==0) {print("IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF); YES")} else {print("IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF); NO")}  
  
  if(!is.null(age_months) & length(setdiff(mdd_vars, colnames(df)))==0 & length(setdiff(mmf_vars, colnames(df)))==0) {print("IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD); YES")} else {print("IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD); NO")} 
 
  flesh_foods_vars <- c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m")
  if(!is.null(age_months) & length(setdiff(flesh_foods_vars, colnames(df)))==0) {print("IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF); YES")} else {print("IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF); NO")}
  
  swt_bv_vars <- c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt")
  if(!is.null(age_months) & length(setdiff(swt_bv_vars, colnames(df)))==0) {print("IYCF Indicator 13: Sweet Beverage Consumption 6-23 months; YES")} else {print("IYCF Indicator 13: Sweet Beverage Consumption 6-23 months; NO")}
  
  unhealthy_food_vars <- c("iycf_7p", "iycf_7q")
  if(!is.null(age_months) & length(setdiff(unhealthy_food_vars, colnames(df)))==0) {print("IYCF Indicator 14: Unehalthy Food Consumption 6-23 months (UFC); YES")} else {print("IYCF Indicator 14: Unhealthy Food Consumption; NO")}
  
  zero_veg_fruit_vars <- c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h")
  if(!is.null(age_months) & length(setdiff(zero_veg_fruit_vars, colnames(df)))==0) {print("IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF); YES")} else {print("IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF);; NO")}
  
  if(!is.null(age_months) & !is.null(iycf_5)) {print("IYCF Indicator 16: Bottle Feeding 0-23 months; YES")} else {print("IYCF Indicator 16: Bottle Feeding 0-23 months;; NO")}
  if(!is.null(age_months) & length(setdiff(ebf_vars, colnames(df)))==0) {print("Infant Feeding Area Graphs; YES")} else {print("Infant Feeding Area Graphs; NO")}
  
  # checks for appropriate types
  
  
  # if any minimal indicators can be calculated, calls re-formatting function
  
  #df <- format_iycf_v2021(df)
  #df <- calculate_iycf_indicators(df)
  return(df)
  
}


format_iycf_v2021 <- function(df) {

  ebf_vars <- c("iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  mdd_vars <- c("iycf_4", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o")
  mmf_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_8")
  mmff_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_7a_num")
  flesh_foods_vars <- c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m")
  swt_bv_vars <- c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt")
  unhealthy_food_vars <- c("iycf_7p", "iycf_7q")
  zero_veg_fruit_vars <- c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h")
  
  # Checking AGE variables
  
  if(c("dob") %in% names(df)) {
    
    if(is.character(df$dob)) {df <- df %>% mutate(dob = ifelse(dob == "", NA, dob))}
    
    df <- df %>%
      mutate(dob = as_date(dob)) %>%
      mutate(month = lubridate::month(dob),
             day = lubridate::day(dob),
             year = lubridate::year(dob),
             dob = NULL,
             dob = paste(month, day, year, sep = "/"),
             day = NULL, month = NULL, year = NULL,
             dob = ifelse(dob == "NA/NA/NA", NA, dob))
    
    
  }
  
  if(c("age_days") %in% names(df)) {
    
    df <- df %>% 
      mutate(age_months = age_days*(365.25/12),
             age_years = floor(age_months/12),
             age_proxy = ifelse(is.na(age_months), NA, 
                                ifelse(age_months <6, "under_6months",
                                       ifelse(age_months<24, "between_6to23months",
                                              ifelse(age_months<60, "between_24to59months", "greater_59months")))))
    
  } else if(c("age_months") %in% names(df)) {
    
    df <- df %>%
      mutate(age_months = as.numeric(age_months),
             age_years = floor((age_months/12)),
             age_proxy = ifelse(is.na(age_months), NA, 
                                ifelse(age_months <6, "under_6months",
                                       ifelse(age_months<24, "between_6to23months",
                                              ifelse(age_months<60, "between_24to59months", "greater_59months")))),
             age_days = age_months*(365.25/12))
    
    
  } else {stop("No age information was given. Please include the appropriate age info.")}   
  
# RECODING SEX VARIABLE

sex_codes <- unique(df$sex)
ideal_codes <- c("1", "2")
sex_recodes <- c("1", "2", "NA")

if(length(setdiff(sex_codes, ideal_codes))==0) {
  print("Good - Sex coded as 1/2 for male/female")
} else { 
  print(cat(paste0("\n Please define how each value is coded for sex in the data, type either '1' for male, 2 for 'female' or 'NA' for missing. \n")))
  for(i in 1:length(sex_codes)) {
    a <- readline(paste0("Is '",sex_codes[[i]], "' coded as male or female? Please input either '1' for male, 2 for 'female' or 'NA' for missing. " ))
    while(length(setdiff(a, sex_recodes))==1) {
      a <- readline(paste0("Invalid input. ", "How is '", sex_codes[[i]], "' for sex coded? Please choose either '1' for male, 2 for 'female' or 'NA' for missing."))
    }
    
    print(paste("The input ", a, " is replacing", sex_codes[[i]]))
    
    if(!is.na(sex_codes[[i]])){
      if(a == "NA") {df <- df %>% mutate(sex = ifelse(sex == sex_codes[[i]], NA, sex))
      } else {df <- df %>% mutate(sex = ifelse(sex == sex_codes[[i]], a, sex))}
    }
    
  }
}
  
# Checking IYCF variable coding
  
  ideal_codes <- c("1", "2", "9")
  iycf_yn_recodes <- c("1", "2", "9", "NA")
  
### Checking IYCF 1 
  
  if(c("iycf_1") %in% colnames(df)) {
   
    iycf_yn_codes <- unique(df$iycf_1)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_1 is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_1 asks if the child was EVER breastfed.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_1 in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_1 coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_1 = ifelse(iycf_1 == iycf_yn_codes[[i]], NA, iycf_1))
          } else {df <- df %>% mutate(iycf_1 = ifelse(iycf_1 == iycf_yn_codes[[i]], a, iycf_1))}
        }
      }
    }
  }
  
### Checking IYCF 2 
  
  if(c("iycf_2") %in% colnames(df)) {
    
    ideal_ei_codes <- c("1", "2", "3", "4", "9")
    iycf_ei_recodes <- c("1", "2", "3", "4", "NA")
    iycf_ei_codes <- unique(df$iycf_2)
    
    if(length(setdiff(iycf_ei_codes, ideal_ei_codes))==0) {
      print("Good - IYCF_2 is coded as 1/2/3/4/9 for immediately/within first hour/within first day/after first day/dont know no response.")
    } else {
      print((paste0("IYCF_2 asks how soon after birth the child was put to the breast.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_2 in the data, type either '1' for immediately, '2' within the first hour, '3' within the first day, '4' for after the first day, '9' for no response or don't know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_ei_codes)) {
        a <- readline(paste0("Is '",iycf_ei_codes[[i]], "' coded as immediately, within first hour, within first day, or after first day? Please input either '1' for immediately, '2' within the first hour, '3' within the first day, '4' for after the first day, '9' for no response or don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_ei_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_ei_codes[[i]], "' for IYCF_2 coded? Please input either '1' for immediately, '2' within the first hour, '3' within the first day, '4' for after the first day, '9' for no response or don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_ei_codes[[i]]))
        
        if(!is.na(iycf_ei_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_2 = ifelse(iycf_2 == iycf_ei_codes[[i]], NA, iycf_2))
          } else {df <- df %>% mutate(iycf_2 = ifelse(iycf_2 == iycf_ei_codes[[i]], a, iycf_2))}
        }
      }
    }
  }
  
  ### Checking IYCF 3 
  
  if(c("iycf_3") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_3)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_3 is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_3 asks if the child was given anything other than breastmilk to eat or drink within the first two days after delivery, including water, infant formula, etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_3 in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_3 coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_3 = ifelse(iycf_3 == iycf_yn_codes[[i]], NA, iycf_3))
          } else {df <- df %>% mutate(iycf_3 = ifelse(iycf_3 == iycf_yn_codes[[i]], a, iycf_3))}
        }
      }
    }
  }
  
  ### Checking IYCF 4 
  
  if(c("iycf_4") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_4)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_4 is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_4 asks if the child was breastfed yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_4 in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_4 coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_4 = ifelse(iycf_4 == iycf_yn_codes[[i]], NA, iycf_4))
          } else {df <- df %>% mutate(iycf_4 = ifelse(iycf_4 == iycf_yn_codes[[i]], a, iycf_4))}
        }
      }
    }
  }
  
  ### Checking IYCF 5 
  
  if(c("iycf_5") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_5)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_5 is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_5 asks if the child drank anything from a bottle with a nipple yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_5 in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_5 coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_5 = ifelse(iycf_5 == iycf_yn_codes[[i]], NA, iycf_5))
          } else {df <- df %>% mutate(iycf_5 = ifelse(iycf_5 == iycf_yn_codes[[i]], a, iycf_5))}
        }
      }
    }
  }
  
  ### Checking IYCF 6a - Plain water
  
  if(c("iycf_6a") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6a)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6a is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6a asks if the child drank any plain water yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6a in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6a coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6a = ifelse(iycf_6a == iycf_yn_codes[[i]], NA, iycf_6a))
          } else {df <- df %>% mutate(iycf_6a = ifelse(iycf_6a == iycf_yn_codes[[i]], a, iycf_6a))}
        }
      }
    }
  }
  
  ### Checking IYCF 6b - Infant Formula
  
  if(c("iycf_6b") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6b)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6b is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6b asks yes/no if the child consumed any infant formula yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6b in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6b coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6b = ifelse(iycf_6b == iycf_yn_codes[[i]], NA, iycf_6b))
          } else {df <- df %>% mutate(iycf_6b = ifelse(iycf_6b == iycf_yn_codes[[i]], a, iycf_6b))}
        }
      }
    }
  }
  
  ### Checking IYCF 6b Num - Number of times infant formula
  
  if(c("iycf_6b_num") %in% colnames(df)) {
    test_for_NAs <- df %>% dplyr::select(iycf_6b_num) %>% filter(!is.na(iycf_6b_num))
    print((paste0("IYCF_6b_num asks the number of times the child consumed any infant formula yesterday during the day or night.")))
    if(TRUE %in% !is.na(suppressWarnings(as.numeric(as.character(test_for_NAs))))) {
      stop("There are non-numeric characters in IYCF_6b_num. Please re-check your input.")
      } else if(!is.numeric(df$iycf_6b_num)) { df <- df %>% mutate(iycf_6b_num = as.numeric(iycf_6b_num))}
  }
  
  ### Checking IYCF 6c - Milk
  
  if(c("iycf_6c") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6c)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6c is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6c asks yes/no if the child consumed any animal milks yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6c in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6c coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6c = ifelse(iycf_6c == iycf_yn_codes[[i]], NA, iycf_6c))
          } else {df <- df %>% mutate(iycf_6c = ifelse(iycf_6c == iycf_yn_codes[[i]], a, iycf_6c))}
        }
      }
    }
  }
  
  ### Checking IYCF 6c Num - Number of times milk
  
  if(c("iycf_6c_num") %in% colnames(df)) {
    test_for_NAs <- df %>% dplyr::select(iycf_6c_num) %>% filter(!is.na(iycf_6c_num))
    print((paste0("IYCF_6c_num asks the number of times the child consumed any animal milks yesterday during the day or night.")))
    if(TRUE %in% !is.na(suppressWarnings(as.numeric(as.character(test_for_NAs))))) {
      stop("There are non-numeric characters in IYCF_6c_num. Please re-check your input.")
    } else if(!is.numeric(df$iycf_6c_num)) { df <- df %>% mutate(iycf_6c_num = as.numeric(iycf_6c_num))}
  }
  
  ### Checking IYCF 6c swt - sweetened animal milks
  
  if(c("iycf_6c_swt") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6c_swt)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6c_swt is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6c_swt asks yes/no if the animal milk consumed by the child was sweetened or not.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6c_swt in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6c_swt coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6c_swt = ifelse(iycf_6c_swt == iycf_yn_codes[[i]], NA, iycf_6c_swt))
          } else {df <- df %>% mutate(iycf_6c_swt = ifelse(iycf_6c_swt == iycf_yn_codes[[i]], a, iycf_6c_swt))}
        }
      }
    }
  }
  
  ### Checking IYCF 6d - Yoghurt drinks
  
  if(c("iycf_6d") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6d)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6d is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6d asks yes/no if the child consumed any yoghurt drinks yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6d in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6d coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6d = ifelse(iycf_6d == iycf_yn_codes[[i]], NA, iycf_6d))
          } else {df <- df %>% mutate(iycf_6d = ifelse(iycf_6d == iycf_yn_codes[[i]], a, iycf_6d))}
        }
      }
    }
  }
  
  ### Checking IYCF 6d Num - Number of times milk
  
  if(c("iycf_6d_num") %in% colnames(df)) {
    test_for_NAs <- df %>% dplyr::select(iycf_6d_num) %>% filter(!is.na(iycf_6d_num))
    print((paste0("IYCF_6d_num asks the number of times the child consumed any yoghurt drinks yesterday during the day or night.")))
    if(TRUE %in% !is.na(suppressWarnings(as.numeric(as.character(test_for_NAs))))) {
      stop("There are non-numeric characters in IYCF_6c_num. Please re-check your input.")
    } else if(!is.numeric(df$iycf_6d_num)) { df <- df %>% mutate(iycf_6d_num = as.numeric(iycf_6d_num))}
  }
  
  ### Checking IYCF 6d swt - sweetened yoghurt drinks
  
  if(c("iycf_6d_swt") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6d_swt)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6d_swt is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6d_swt asks yes/no if the yoghurt drinks consumed by the child was sweetened or not.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6d_swt in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6d_swt coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6d_swt = ifelse(iycf_6d_swt == iycf_yn_codes[[i]], NA, iycf_6d_swt))
          } else {df <- df %>% mutate(iycf_6d_swt = ifelse(iycf_6d_swt == iycf_yn_codes[[i]], a, iycf_6d_swt))}
        }
      }
    }
  }
  
  ### Checking IYCF 6e - Chocolate flavoured drinks
  
  if(c("iycf_6e") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6e)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6e is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6e asks yes/no if the child consumed chocolate flavoured drinks yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6e in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6e coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6e = ifelse(iycf_6e == iycf_yn_codes[[i]], NA, iycf_6e))
          } else {df <- df %>% mutate(iycf_6e = ifelse(iycf_6e == iycf_yn_codes[[i]], a, iycf_6e))}
        }
      }
    }
  }
  
  ### Checking IYCF 6f - Fruit juice or fruit flavoured drinks
  
  if(c("iycf_6f") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6f)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6f is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6f asks yes/no if the child consumed fruit juice or fruit flavoured drinks yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6f in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6f coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6f = ifelse(iycf_6f == iycf_yn_codes[[i]], NA, iycf_6f))
          } else {df <- df %>% mutate(iycf_6f = ifelse(iycf_6f == iycf_yn_codes[[i]], a, iycf_6f))}
        }
      }
    }
  }
  
  ### Checking IYCF 6g - Sodas, malts, sports and energy drinks
  
  if(c("iycf_6g") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6g)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6g is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6g asks yes/no if the child consumed sodas, malts, sports or energy drinks yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6g in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6g coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6g = ifelse(iycf_6g == iycf_yn_codes[[i]], NA, iycf_6g))
          } else {df <- df %>% mutate(iycf_6g = ifelse(iycf_6g == iycf_yn_codes[[i]], a, iycf_6g))}
        }
      }
    }
  }
  
  ### Checking IYCF 6h - Coffees, tea, herbal drinks
  
  if(c("iycf_6h") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6h)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6h is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6h asks yes/no if the child consumed tea, coffee or herbal tea yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6h in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6h coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6h = ifelse(iycf_6h == iycf_yn_codes[[i]], NA, iycf_6h))
          } else {df <- df %>% mutate(iycf_6h = ifelse(iycf_6h == iycf_yn_codes[[i]], a, iycf_6h))}
        }
      }
    }
  }
  
  ### Checking IYCF 6h swt - Sweetened Coffees, tea, herbal drinks
  
  if(c("iycf_6h_swt") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6h_swt)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6h is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6h asks yes/no if the tea/coffee/herbal drink consumed by the child was sweetened.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6h in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6h coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6h_swt = ifelse(iycf_6h_swt == iycf_yn_codes[[i]], NA, iycf_6h_swt))
          } else {df <- df %>% mutate(iycf_6h_swt = ifelse(iycf_6h_swt == iycf_yn_codes[[i]], a, iycf_6h_swt))}
        }
      }
    }
  }
  
  ### Checking IYCF 6I - Clear broth or soup
  
  if(c("iycf_6i") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6i)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6i is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6i asks yes/no if the child consumed clear broth or soup yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6i in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6i coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6i = ifelse(iycf_6i == iycf_yn_codes[[i]], NA, iycf_6i))
          } else {df <- df %>% mutate(iycf_6i = ifelse(iycf_6i == iycf_yn_codes[[i]], a, iycf_6i))}
        }
      }
    }
  }
  
  ### Checking IYCF 6j - Any other liquids
  
  if(c("iycf_6j") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6j)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6j is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6j asks yes/no if the child consumed any OTHER liquids yesterday during the day or night.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6j in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6j coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6j = ifelse(iycf_6j == iycf_yn_codes[[i]], NA, iycf_6j))
          } else {df <- df %>% mutate(iycf_6j = ifelse(iycf_6j == iycf_yn_codes[[i]], a, iycf_6j))}
        }
      }
    }
  }
  
  ### Checking IYCF 6j swt - sweetened Any other liquids
  
  if(c("iycf_6j_swt") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_6j_swt)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_6j_Swt is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_6j_Swt asks yes/no if any of the other liquids consumed by the child were sweetened.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_6j_Swt in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_6j_Swt coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_6j_swt = ifelse(iycf_6j_swt == iycf_yn_codes[[i]], NA, iycf_6j_swt))
          } else {df <- df %>% mutate(iycf_6j_swt = ifelse(iycf_6j_swt == iycf_yn_codes[[i]], a, iycf_6j_swt))}
        }
      }
    }
  }
  
  ### Checking IYCF 7a - yoghurt, other than yoghurt drinks
  
  if(c("iycf_7a") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7a)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7a is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7a asks yes/no if the child consumed any yoghurt (not yoghurt drink)")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7a in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7a coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7a = ifelse(iycf_7a == iycf_yn_codes[[i]], NA, iycf_7a))
          } else {df <- df %>% mutate(iycf_7a = ifelse(iycf_7a == iycf_yn_codes[[i]], a, iycf_7a))}
        }
      }
    }
  }
  
  ### Checking IYCF 7a Num - Number times yoghurt (not drink)
  
  if(c("iycf_7a_num") %in% colnames(df)) {
    test_for_NAs <- df %>% dplyr::select(iycf_7a_num) %>% filter(!is.na(iycf_7a_num))
    print((paste0("IYCF_7a_num asks the number of times the child consumed any yoghurt (not drinks) yesterday during the day or night.")))
    if(TRUE %in% !is.na(suppressWarnings(as.numeric(as.character(test_for_NAs))))) {
      stop("There are non-numeric characters in IYCF_6c_num. Please re-check your input.")
    } else if(!is.numeric(df$iycf_7a_num)) { df <- df %>% mutate(iycf_7a_num = as.numeric(iycf_7a_num))}
  }
  
  ### Checking IYCF 7b - Cereals
  
  if(c("iycf_7b") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7b)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7b is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7b asks yes/no if the child consumed any cereals like porridge, bread, rice, pasta, etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7b in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7b coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7b = ifelse(iycf_7b == iycf_yn_codes[[i]], NA, iycf_7b))
          } else {df <- df %>% mutate(iycf_7b = ifelse(iycf_7b == iycf_yn_codes[[i]], a, iycf_7b))}
        }
      }
    }
  }
  
  ### Checking IYCF 7c - Vitamin A rich vegetables
  
  if(c("iycf_7c") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7c)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7c is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7c asks yes/no if the child consumed any vitamin A rich vegetables like pumpkin, carrots, sweet red peppers, yellow/orange sweet potato, etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7c in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7c coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7c = ifelse(iycf_7c == iycf_yn_codes[[i]], NA, iycf_7c))
          } else {df <- df %>% mutate(iycf_7c = ifelse(iycf_7c == iycf_yn_codes[[i]], a, iycf_7c))}
        }
      }
    }
  }
  
  ### Checking IYCF 7d - Roots and tubers
  
  if(c("iycf_7d") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7d)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7d is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7d asks yes/no if the child consumed any roots or tubers like plantains, white potatoes, white yams, etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7d in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7d coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7d = ifelse(iycf_7d == iycf_yn_codes[[i]], NA, iycf_7d))
          } else {df <- df %>% mutate(iycf_7d = ifelse(iycf_7d == iycf_yn_codes[[i]], a, iycf_7d))}
        }
      }
    }
  }
  
  ### Checking IYCF 7e - Dark Green Leafy Vegetables
  
  if(c("iycf_7e") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7e)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7e is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7e asks yes/no if the child consumed any dark green leafy vegetables. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7e in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7e coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7e = ifelse(iycf_7e == iycf_yn_codes[[i]], NA, iycf_7e))
          } else {df <- df %>% mutate(iycf_7e = ifelse(iycf_7e == iycf_yn_codes[[i]], a, iycf_7e))}
        }
      }
    }
  }
  
  ### Checking IYCF 7f - Other vegetables
  
  if(c("iycf_7f") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7f)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7f is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7f asks yes/no if the child consumed any OTHER vegetables. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7f in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7f coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7f = ifelse(iycf_7f == iycf_yn_codes[[i]], NA, iycf_7f))
          } else {df <- df %>% mutate(iycf_7f = ifelse(iycf_7f == iycf_yn_codes[[i]], a, iycf_7f))}
        }
      }
    }
  }
  
  ### Checking IYCF 7g - Vitamin A rich fruits
  
  if(c("iycf_7g") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7g)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7g is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7g asks yes/no if the child consumed any vitamin A rich fruits like ripe mango, ripe papaya, etc.. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7g in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7g coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7g = ifelse(iycf_7g == iycf_yn_codes[[i]], NA, iycf_7g))
          } else {df <- df %>% mutate(iycf_7g = ifelse(iycf_7g == iycf_yn_codes[[i]], a, iycf_7g))}
        }
      }
    }
  }
  
  ### Checking IYCF 7h - Other fruits
  
  if(c("iycf_7h") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7h)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7h is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7h asks yes/no if the child consumed any OTHER fruits. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7h in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7h coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7h = ifelse(iycf_7h == iycf_yn_codes[[i]], NA, iycf_7h))
          } else {df <- df %>% mutate(iycf_7h = ifelse(iycf_7h == iycf_yn_codes[[i]], a, iycf_7h))}
        }
      }
    }
  }
  
  ### Checking IYCF 7i - Organ meats
  
  if(c("iycf_7i") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7i)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7i is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7i asks yes/no if the child consumed any organ meats like liver, heart, kidney, etc. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7i in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7i coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7i = ifelse(iycf_7i == iycf_yn_codes[[i]], NA, iycf_7i))
          } else {df <- df %>% mutate(iycf_7i = ifelse(iycf_7i == iycf_yn_codes[[i]], a, iycf_7i))}
        }
      }
    }
  }
  
  ### Checking IYCF 7j - Processed meats
  
  if(c("iycf_7j") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7j)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7j is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7j asks yes/no if the child consumed any processed meats like sausages, hot dogs, bacon, canned meat, etc. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7j in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7j coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7j = ifelse(iycf_7j == iycf_yn_codes[[i]], NA, iycf_7j))
          } else {df <- df %>% mutate(iycf_7j = ifelse(iycf_7j == iycf_yn_codes[[i]], a, iycf_7j))}
        }
      }
    }
  }
  
  ### Checking IYCF 7k - Other meats
  
  if(c("iycf_7k") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7k)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7k is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7k asks yes/no if the child consumed any other meats like beef, pork, lamb, chicken, goat, etc. ")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7k in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7k coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7k = ifelse(iycf_7k == iycf_yn_codes[[i]], NA, iycf_7k))
          } else {df <- df %>% mutate(iycf_7k = ifelse(iycf_7k == iycf_yn_codes[[i]], a, iycf_7k))}
        }
      }
    }
  }
  
  ### Checking IYCF 7l - Eggs
  
  if(c("iycf_7l") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7l)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7l is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7l asks yes/no if the child consumed any eggs.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7l in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7l coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7l = ifelse(iycf_7l == iycf_yn_codes[[i]], NA, iycf_7l))
          } else {df <- df %>% mutate(iycf_7l = ifelse(iycf_7l == iycf_yn_codes[[i]], a, iycf_7l))}
        }
      }
    }
  }
  
  ### Checking IYCF 7m - Fish
  
  if(c("iycf_7m") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7m)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7m is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7m asks yes/no if the child consumed any fish, dried fish or other fish products")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7m in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7m coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7m = ifelse(iycf_7m == iycf_yn_codes[[i]], NA, iycf_7m))
          } else {df <- df %>% mutate(iycf_7m = ifelse(iycf_7m == iycf_yn_codes[[i]], a, iycf_7m))}
        }
      }
    }
  }
  
  ### Checking IYCF 7n - Legumes
  
  if(c("iycf_7n") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7n)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7n is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7n asks yes/no if the child consumed any legumes like beans, peas, lentils, nuts, seeds, etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7n in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7n coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7n = ifelse(iycf_7n == iycf_yn_codes[[i]], NA, iycf_7n))
          } else {df <- df %>% mutate(iycf_7n = ifelse(iycf_7n == iycf_yn_codes[[i]], a, iycf_7n))}
        }
      }
    }
  }
  
  ### Checking IYCF 7o - Cheeses
  
  if(c("iycf_7o") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7o)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7o is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7o asks yes/no if the child consumed any hard or soft cheeses etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7o in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7o coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7o = ifelse(iycf_7o == iycf_yn_codes[[i]], NA, iycf_7o))
          } else {df <- df %>% mutate(iycf_7o = ifelse(iycf_7o == iycf_yn_codes[[i]], a, iycf_7o))}
        }
      }
    }
  }
  
  ### Checking IYCF 7p - Sweet foods
  
  if(c("iycf_7p") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7p)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7p is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7p asks yes/no if the child consumed any sweet foods like chocolates, sweets, cakes, biscuits, ice cream, etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7p in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7p coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7p = ifelse(iycf_7p == iycf_yn_codes[[i]], NA, iycf_7p))
          } else {df <- df %>% mutate(iycf_7p = ifelse(iycf_7p == iycf_yn_codes[[i]], a, iycf_7p))}
        }
      }
    }
  }
  
  ### Checking IYCF 7q - Fried carbs
  
  if(c("iycf_7q") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7q)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7q is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7q asks yes/no if the child consumed any chips, crisps, puffs, french fries, other fried carbs etc.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7q in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7q coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7q = ifelse(iycf_7q == iycf_yn_codes[[i]], NA, iycf_7q))
          } else {df <- df %>% mutate(iycf_7q = ifelse(iycf_7q == iycf_yn_codes[[i]], a, iycf_7q))}
        }
      }
    }
  }
  
  ### Checking IYCF 7r - Other solid semi-solid or soft foods
  
  if(c("iycf_7r") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7r)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7r is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7r asks yes/no if the child consumed any other solid, semi-solid or soft foods.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7r in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7r coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7r = ifelse(iycf_7r == iycf_yn_codes[[i]], NA, iycf_7r))
          } else {df <- df %>% mutate(iycf_7r = ifelse(iycf_7r == iycf_yn_codes[[i]], a, iycf_7r))}
        }
      }
    }
  }
  
  ### Checking IYCF 7s - Did child eat any solid, semi-solid or soft foods (y/n)
  
  if(c("iycf_7s") %in% colnames(df)) {
    
    iycf_yn_codes <- unique(df$iycf_7s)
    
    if(length(setdiff(iycf_yn_codes, ideal_codes))==0) {
      print("Good - IYCF_7s is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("IYCF_7s asks yes/no if the child consumed any other solid, semi-solid or soft foods.")))
      print(cat(paste0("\n Please define how each value is coded for IYCF_7s in the data, type either '1' for yes, '2' for no, '9' for no response or dont know, or 'NA' for missing. \n")))
      for(i in 1:length(iycf_yn_codes)) {
        a <- readline(paste0("Is '",iycf_yn_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", iycf_yn_codes[[i]], "' for IYCF_7s coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }
        
        print(paste("The input ", a, " is replacing", iycf_yn_codes[[i]]))
        
        if(!is.na(iycf_yn_codes[[i]])){
          if(a == "NA") {df <- df %>% mutate(iycf_7s = ifelse(iycf_7s == iycf_yn_codes[[i]], NA, iycf_7s))
          } else {df <- df %>% mutate(iycf_7s = ifelse(iycf_7s == iycf_yn_codes[[i]], a, iycf_7s))}
        }
      }
    }
  }
  
  ### Checking IYCF 8 - Number times ate solid, semi-solid or soft foods yesterday
  
  if(c("iycf_8") %in% colnames(df)) {
    test_for_NAs <- df %>% dplyr::select(iycf_8) %>% filter(!is.na(iycf_8))
    print((paste0("IYCF_8 asks the number of times the child consumed any solid, semi-solid or soft foods yesterday.")))
    if(TRUE %in% !is.na(suppressWarnings(as.numeric(as.character(test_for_NAs))))) {
      stop("There are non-numeric characters in IYCF_6c_num. Please re-check your input.")
    } else if(!is.numeric(df$iycf_8)) { df <- df %>% mutate(iycf_8 = as.numeric(iycf_8))}
  }
  
  # Other coding fixes
  
  if(length(setdiff(c("iycf_1", "iycf_4"), colnames(df)))== 0) {df <- df %>% mutate(iycf_4 = ifelse(iycf_1 == 2, 2, iycf_4))}
  
  iycf_vars <- c("iycf_1", "iycf_2", "iycf3", "iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  
  #vars_to_recode <- setdiff(iycf_vars, )
  
  
  return(df)
  
}

calculate_iycf_indicators <- function(df, use_flags = NULL) {
  
  if(!hasArg(use_flags)) {stop("Plese include the the use_flags argument, with either 'yes' or 'no' in quotes.")}
  
  if(length(setdiff(use_flags, c("yes", "no")))!=0) {stop("Please input 'yes' or 'no' for the use_flags argument. ")}
  
  if(!is.null(use_flags) & use_flags == "yes") {df <- flag_iycf_issues_v2021(df)}
  
  ebf_vars <- c("age_months", "iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  
  isssf_vars <- c("age_months", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  
  mdd_vars <- c("age_months", "iycf_4", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o")
  mmf_vars <- c("age_months", "iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_8")
  mmff_vars <- c("age_months","iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_7a_num")
  flesh_foods_vars <- c("age_months","iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m")
  swt_bv_vars <- c("age_months","iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt")
  unhealthy_food_vars <- c("age_months","iycf_7p", "iycf_7q")
  zero_veg_fruit_vars <- c("age_months","iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h")
  
  # Set all flagged children to missing for IYCF cols
  
  
  # "IYCF Indicator 1: Ever Breastfed; YES"

  if(length(setdiff(c("iycf_1", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_1", "age_months")] <- sapply(df[c("iycf_1", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_evbf = ifelse(is.na(age_months) | age_months >23, NA, ifelse(is.na(iycf_1), 0, ifelse(iycf_1 == 1, 1, 0))))
    
  }
  
  # "IYCF Indicator 2: Early Initiation of Breastfeeding;
  
  if(length(setdiff(c("iycf_2", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_2", "age_months")] <- sapply(df[c("iycf_2", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_eibf = ifelse(is.na(age_months) | age_months >23, NA, ifelse(is.na(iycf_2), 0, ifelse(iycf_2 == 1 | iycf_2 == 2, 1, 0))))
    #'1' for immediately, '2' within the first hour, '3' within the first day, '4' for after the first day, '9' for no response or don't know, or 'NA' for missing.
  }
  
  # "IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth;
  
  if(length(setdiff(c("iycf_3", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_3", "age_months")] <- sapply(df[c("iycf_3", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_ebf2d = ifelse(is.na(age_months) | age_months >23, NA, ifelse(is.na(iycf_3), 0 , ifelse(iycf_3 == 2, 1, 0))))
    
  }
  
  # "IYCF Indicator 4: Exclusive Breastfeeding;
  
  if(length(setdiff(ebf_vars, colnames(df)))==0) {
    
    df[ebf_vars] <- sapply(df[ebf_vars], as.numeric)
    
    df <- df %>% 
      mutate(iycf_ebf = ifelse(is.na(age_months) | age_months > 5, NA, 
                               ifelse(is.na(iycf_6a) | is.na(iycf_6b) | is.na(iycf_6c) | is.na(iycf_6d) | is.na(iycf_6e) | is.na(iycf_6f) | is.na(iycf_6g) | is.na(iycf_6h) | is.na(iycf_6i) | is.na(iycf_6j) | is.na(iycf_7a) | is.na(iycf_7b) | is.na(iycf_7c) | is.na(iycf_7d) | is.na(iycf_7e) | is.na(iycf_7f) | is.na(iycf_7g) | is.na(iycf_7h) | is.na(iycf_7i) | is.na(iycf_7j) | is.na(iycf_7k) | is.na(iycf_7l) | is.na(iycf_7m) | is.na(iycf_7n) | is.na(iycf_7o) | is.na(iycf_7p) | is.na(iycf_7q) | is.na(iycf_7r), 0,
                                      ifelse(iycf_6a == 2 & iycf_6b == 2 & iycf_6c == 2 & iycf_6d == 2 & iycf_6e == 2 & iycf_6f == 2 & iycf_6g == 2 & iycf_6h == 2 & iycf_6i == 2 & iycf_6j == 2 & iycf_7a == 2 & iycf_7b == 2 & iycf_7c == 2 & iycf_7d == 2 & iycf_7e == 2 & iycf_7f == 2 & iycf_7g == 2 & iycf_7h == 2 & iycf_7i == 2 & iycf_7j == 2 & iycf_7k== 2 & iycf_7l == 2 & iycf_7m == 2 & iycf_7n == 2 & iycf_7o == 2 & iycf_7p == 2 & iycf_7q == 2 & iycf_7r == 2, 1, 0))),
             )   
    
    if(!is.null(use_flags) & use_flags == "yes") {df <- df %>% mutate(iycf_ebf = ifelse(is.na(flag_all_liquids), iycf_ebf, ifelse(flag_all_liquids == 1, 0, iycf_ebf)))}
    
  }
  
  # "IYCF Indicator 5: Mixed Milk Feeding (MIxMF)
  
  if(length(setdiff(c("iycf_4", "iycf_6b", "iycf_6c", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_4", "iycf_6b", "iycf_6c", "age_months")] <- sapply(df[c("iycf_4", "iycf_6b", "iycf_6c", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_mixmf = ifelse(is.na(age_months) | age_months >5, NA, ifelse(is.na(iycf_4) | is.na(iycf_6b) | is.na(iycf_6c) | is.na(iycf_6c), 0, ifelse(iycf_4 == 1 & (iycf_6b == 1 | iycf_6c == 1), 1, 0))))
    
  }
  
  # "IYCF Indicator 6: Continued Breastfeeding 12-23 months; 
  
  if(length(setdiff(c("iycf_4", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_4", "age_months")] <- sapply(df[c("iycf_4", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_cbf = ifelse(is.na(age_months) | age_months < 12 | age_months >23, NA, ifelse(is.na(iycf_4), 0, ifelse(iycf_4 == 1, 1, 0))))
    
  }
  
  # "IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods (ISSSF)

  if(length(setdiff(isssf_vars, colnames(df)))==0) {
    
    df[isssf_vars] <- sapply(df[isssf_vars], as.numeric)
    
    df <- df %>% 
      mutate(iycf_isssf = ifelse(is.na(age_months) | age_months < 6 | age_months >8, NA, ifelse(is.na(iycf_7a) | is.na(iycf_7b) | is.na(iycf_7c) | is.na(iycf_7d) | is.na(iycf_7e) | is.na(iycf_7f) | is.na(iycf_7g) | is.na(iycf_7h) | is.na(iycf_7i) | is.na(iycf_7j) | is.na(iycf_7k) | is.na(iycf_7l) | is.na(iycf_7m) | is.na(iycf_7n) | is.na(iycf_7o) | is.na(iycf_7p) | is.na(iycf_7q) | is.na(iycf_7r), 0,
                                                                                                ifelse(iycf_7a == 1 | iycf_7b == 1 | iycf_7c == 1 | iycf_7d == 1 | iycf_7e == 1 | iycf_7f == 1 | iycf_7g == 1 | iycf_7h == 1 | iycf_7i == 1 | iycf_7j == 1 | iycf_7k == 1 | iycf_7l == 1 | iycf_7m == 1 | iycf_7n == 1 | iycf_7o == 1 | iycf_7p == 1 | iycf_7q == 1 | iycf_7r == 1, 1, 0))))
    
  }
  
  # "IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD);
  
  if(length(setdiff(mdd_vars, colnames(df)))==0) {

    df[mdd_vars] <- sapply(df[mdd_vars], as.numeric)
    
    df <- df %>% 
      mutate(mdd1 = ifelse(is.na(iycf_4), NA, ifelse(iycf_4 == 1, 1, 0)),
             mdd2 = ifelse(is.na(iycf_7b), NA, ifelse(iycf_7b == 1, 1, 0)),
             mdd2 = ifelse(is.na(iycf_7d) & is.na(mdd2), NA, ifelse(is.na(iycf_7d), mdd2, ifelse(iycf_7d == 1, 1, ifelse(is.na(mdd2), 0, mdd2)))),
             mdd3 = ifelse(is.na(iycf_7n), NA, ifelse(iycf_7n == 1, 1, 0)),
             mdd4 = ifelse(is.na(iycf_6b), NA, ifelse(iycf_6b == 1, 1, 0)),
             mdd4 = ifelse(is.na(iycf_6c) & is.na(mdd4), NA, ifelse(is.na(iycf_6c), mdd4, ifelse(iycf_6c == 1, 1, ifelse(is.na(mdd4), 0, mdd4)))),
             mdd4 = ifelse(is.na(iycf_6d) & is.na(mdd4), NA, ifelse(is.na(iycf_6d), mdd4, ifelse(iycf_6d == 1, 1, ifelse(is.na(mdd4), 0, mdd4)))),
             mdd4 = ifelse(is.na(iycf_7a) & is.na(mdd4), NA, ifelse(is.na(iycf_7a), mdd4, ifelse(iycf_7a == 1, 1, ifelse(is.na(mdd4), 0, mdd4)))),
             mdd4 = ifelse(is.na(iycf_7o) & is.na(mdd4), NA, ifelse(is.na(iycf_7o), mdd4, ifelse(iycf_7o == 1, 1, ifelse(is.na(mdd4), 0, mdd4)))),
             mdd5 = ifelse(is.na(iycf_7i), NA, ifelse(iycf_7i == 1, 1, 0)),
             mdd5 = ifelse(is.na(iycf_7j) & is.na(mdd5), NA, ifelse(is.na(iycf_7j), mdd5, ifelse(iycf_7j == 1, 1, ifelse(is.na(mdd5), 0, mdd5)))),
             mdd5 = ifelse(is.na(iycf_7k) & is.na(mdd5), NA, ifelse(is.na(iycf_7k), mdd5, ifelse(iycf_7k == 1, 1, ifelse(is.na(mdd5), 0, mdd5)))),
             mdd5 = ifelse(is.na(iycf_7m) & is.na(mdd5), NA, ifelse(is.na(iycf_7m), mdd5, ifelse(iycf_7m == 1, 1, ifelse(is.na(mdd5), 0, mdd5)))),
             mdd6 = ifelse(is.na(iycf_7l), NA, ifelse(iycf_7l == 1, 1, 0)),
             mdd7 = ifelse(is.na(iycf_7c), NA, ifelse(iycf_7c == 1, 1, 0)),
             mdd7 = ifelse(is.na(mdd7) & is.na(iycf_7e), NA, ifelse(is.na(iycf_7e), mdd7, ifelse(iycf_7e == 1, 1, ifelse(is.na(mdd7), 0, mdd7)))),
             mdd7 = ifelse(is.na(mdd7) & is.na(iycf_7g), NA, ifelse(is.na(iycf_7g), mdd7, ifelse(iycf_7g == 1, 1, ifelse(is.na(mdd7), 0, mdd7)))),
             mdd8 = ifelse(is.na(iycf_7f), NA, ifelse(iycf_7f == 1, 1, 0)),
             mdd8 = ifelse(is.na(iycf_7h) & is.na(mdd8), NA, ifelse(is.na(iycf_7h), mdd8, ifelse(iycf_7h == 1, 1, ifelse(is.na(mdd8), 0, mdd8))))) %>%
      rowwise() %>%
      mutate(iycf_mdd_score = sum(c(mdd1, mdd2, mdd3, mdd4, mdd5, mdd6, mdd7, mdd8), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(iycf_mdd_cat = ifelse(is.na(age_months) | age_months <6 | age_months >23, NA, ifelse(iycf_mdd_score >=5 & iycf_mdd_score <10, 1, 0)),
             )
    
    if(!is.null(use_flags) & use_flags == "yes") {
      df <- df %>%
        mutate(iycf_mdd_score = ifelse(is.na(flag_yes_foods), iycf_mdd_score, ifelse(flag_yes_foods == 1, NA, iycf_mdd_score)),
               iycf_mdd_score = ifelse(is.na(flag_no_foods), iycf_mdd_score, ifelse(flag_no_foods == 1, NA, iycf_mdd_score)),
               iycf_mdd_cat = ifelse(is.na(flag_no_foods), iycf_mdd_cat, ifelse(flag_no_foods == 1, 0, iycf_mdd_cat)),
               iycf_mdd_cat = ifelse(is.na(flag_yes_foods), iycf_mdd_cat, ifelse(flag_yes_foods == 1, 0, iycf_mdd_cat)))
    }
    
  }
  
  # "IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF);
  
  if(length(setdiff(mmf_vars, colnames(df)))==0) {
    
    df[mmf_vars] <- sapply(df[mmf_vars], as.numeric)
    
    df <- df %>% 
      mutate(mmf_bf_6to8months = ifelse(is.na(age_months) | age_months <6 | age_months >8, NA, ifelse(is.na(iycf_4) | is.na(iycf_8), 0, ifelse(iycf_4 == 1 & age_months >5 & age_months <9 & iycf_8 >=2, 1, 0))),
             mmf_bf_9to23months = ifelse(is.na(age_months) | age_months <9 | age_months >23, NA, ifelse(is.na(iycf_4) | is.na(iycf_8), 0, ifelse(iycf_4 == 1 & age_months >8 & age_months <24 & iycf_8 >=3, 1, 0)))) %>%
      rowwise() %>%
      mutate(count_6b_6c_6d_8 = sum(c(iycf_6b_num, iycf_6c_num, iycf_6d_num, iycf_8), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(mmf_nonbf_6to23months = ifelse(is.na(age_months) | age_months <6 | age_months >23, NA, ifelse(is.na(iycf_4) | iycf_4 == 1, NA, ifelse(is.na(count_6b_6c_6d_8) | is.na(iycf_8), 0, ifelse(iycf_4 == 2 & age_months >5 & age_months <24 & count_6b_6c_6d_8 >=4 & iycf_8 >=1, 1, 0)))),
             iycf_mmf = ifelse(is.na(age_months) | age_months < 6 | age_months >23, NA, 
                               ifelse(mmf_bf_6to8months == 1 & is.na(mmf_bf_9to23months) & is.na(mmf_nonbf_6to23months), 1,
                                      ifelse( is.na(mmf_bf_6to8months) & mmf_bf_9to23months == 1 & is.na(mmf_nonbf_6to23months), 1,
                                              ifelse( is.na(mmf_bf_6to8months) & is.na(mmf_bf_9to23months) & mmf_nonbf_6to23months == 1, 1, 0))))
             )

    
    if(!is.null(use_flags) & use_flags == "yes") {
      df <- df %>%
        mutate(iycf_mmf = ifelse(is.na(flag_no_foods), iycf_mmf, ifelse(flag_no_foods == 1, 0, iycf_mmf)),
               iycf_mmf = ifelse(is.na(flag_yes_foods), iycf_mmf, ifelse(flag_yes_foods == 1, 0, iycf_mmf)),
               iycf_mmf = ifelse(is.na(flag_some_foods_no_meal), iycf_mmf, ifelse(flag_some_foods_no_meal == 1, 0, iycf_mmf)))
    }

    
  }
  
  # "IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF);
  
  if(length(setdiff(mmff_vars, colnames(df)))==0) {
    
    df[mmff_vars] <- sapply(df[mmff_vars], as.numeric)
    
    df <- df %>%
      rowwise() %>%
      mutate(count_dairy = sum(c(iycf_6b_num, iycf_6c_num, iycf_6d_num, iycf_7a_num), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(iycf_mmff = ifelse(is.na(age_months) | age_months < 6 | age_months >23 | is.na(iycf_4) | iycf_4 == 1, NA, ifelse(iycf_4 != 1 & count_dairy >= 2, 1, 0)))
    
  }  

  # "IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD); YES"

  if(length(setdiff(c("iycf_mmf", "iycf_mdd_cat", "iycf_mmff", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_mmf", "iycf_mdd_cat", "iycf_mmff", "age_months")] <- sapply(df[c("iycf_mmf", "iycf_mdd_cat", "iycf_mmff", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_mad = ifelse(is.na(age_months) | age_months < 6 | age_months > 23, NA, ifelse(iycf_mmf == 1 & iycf_mdd_cat == 1 & (iycf_4 == 1 | iycf_mmff == 1), 1, 0)),
             
      )
    
    if(!is.null(use_flags) & use_flags == "yes") {
      
      df <- df %>%
        mutate(iycf_mad = ifelse(is.na(flag_no_foods), iycf_mad, ifelse(flag_no_foods == 1, 0, iycf_mad)),
               iycf_mad = ifelse(is.na(flag_yes_foods), iycf_mad, ifelse(flag_yes_foods == 1, 0, iycf_mad)),
               iycf_mad = ifelse(is.na(flag_some_foods_no_meal), iycf_mad, ifelse(flag_some_foods_no_meal == 1, 0, iycf_mad)))
      
    }
    
    
  }
  
  # "IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF);
  
  if(length(setdiff(c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "age_months")] <- sapply(df[c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_eff = ifelse(is.na(age_months) | age_months < 6 | age_months > 23, NA, ifelse(iycf_7i == 1 | iycf_7j == 1 | iycf_7k == 1 | iycf_7l == 1 | iycf_7m == 1, 1, 0)),
             )
    
    if(!is.null(use_flags) & use_flags == "yes") {
     df <- df %>% mutate(iycf_eff = ifelse(is.na(flag_no_foods), iycf_eff, ifelse(flag_no_foods == 1, 0 , iycf_eff)),
                         iycf_eff = ifelse(is.na(flag_yes_foods), iycf_eff, ifelse(flag_yes_foods == 1, 0, iycf_eff))) 
    }
    
  }

  # "IYCF Indicator 13: Sweet Beverage Consumption 6-23 months (SWB)

  if(length(setdiff(c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt", "age_months")] <- sapply(df[c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_swb = ifelse(is.na(iycf_6c_swt) | is.na(iycf_6d_swt) | is.na(iycf_6e) | is.na(iycf_6f) | is.na(iycf_6g) | is.na(iycf_6h_swt) | is.na(iycf_6j_swt) | age_months < 6 | age_months > 23, NA, ifelse(iycf_6c_swt == 1 | iycf_6d_swt == 1 | iycf_6e == 1 | iycf_6f == 1 | iycf_6g == 1 | iycf_6h_swt == 1 | iycf_6j_swt == 1, 1, 0)),
             )
    
    if(!is.null(use_flags) & use_flags == "yes") {
      df <- df %>% mutate(iycf_swb = ifelse(is.na(flag_yes_liquids), iycf_swb, ifelse(flag_yes_liquids == 1, 0, iycf_swb)))
    }
    
  }
  
  # "IYCF Indicator 14: Unhealthy Food Consumption (UFC)
  
  if(length(setdiff(c("iycf_7p", "iycf_7q", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_7p", "iycf_7q", "age_months")] <- sapply(df[c("iycf_7p", "iycf_7q", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_ufc = ifelse(is.na(iycf_7p) | is.na(iycf_7q) | age_months < 6 | age_months > 23, NA, ifelse(iycf_7p == 1 | iycf_7q == 1, 1, 0)),
             )
    
    if(!is.null(use_flags) & use_flags == "yes") {
      df <- df %>% mutate(iycf_ufc = ifelse(is.na(flag_yes_foods), iycf_ufc, ifelse(flag_yes_foods == 1, 0, iycf_ufc)))
    }
    
  }
  
  # "IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF)
  
  if(length(setdiff(c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g" , "iycf_7h", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g" , "iycf_7h", "age_months")] <- sapply(df[c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g" , "iycf_7h", "age_months")], as.numeric)
    
    df <- df %>%
      
      mutate(zvf1 = ifelse(is.na(iycf_7c), 0, ifelse(iycf_7c == 2, 1, 0)),
             zvf2 = ifelse(is.na(iycf_7e), 0, ifelse(iycf_7e == 2, 1, 0)),
             zvf3 = ifelse(is.na(iycf_7f), 0, ifelse(iycf_7f == 2, 1, 0)),
             zvf4 = ifelse(is.na(iycf_7g), 0, ifelse(iycf_7g == 2, 1, 0)),
             zvf5 = ifelse(is.na(iycf_7h), 0, ifelse(iycf_7h == 2, 1, 0))) %>%
      rowwise() %>%
      mutate(zvf_sum = sum(c(zvf1, zvf2, zvf3, zvf4, zvf5), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(iycf_zvf = ifelse(is.na(zvf_sum), 0, ifelse(zvf_sum == 5, 1, 0)),
             iycf_zvf = ifelse(is.na(age_months) | age_months <6 | age_months >23, NA, iycf_zvf))
    
    if(!is.null(use_flags) & use_flags == "yes") {
      df <- df %>% mutate(iycf_zvf = ifelse(is.na(flag_no_foods), iycf_zvf, ifelse(flag_no_foods == 1, 0, iycf_zvf)),
                          iycf_zvf = ifelse(is.na(flag_yes_foods), iycf_zvf, ifelse(flag_yes_foods == 1, 0, iycf_zvf)))
    }
    
  }
  
  # "IYCF Indicator 16: Bottle Feeding 0-23 months
  
  if(length(setdiff(c("iycf_5", "age_months"), colnames(df)))==0) {
    
    df[c("iycf_5", "age_months")] <- sapply(df[c("iycf_5", "age_months")], as.numeric)
    
    df <- df %>% 
      mutate(iycf_bof = ifelse(is.na(iycf_5) | age_months < 6 | age_months > 23, NA, ifelse(iycf_5 == 1, 1, 0)))
    
  }
  
  return(df)
  
}

flag_iycf_issues_v2021 <- function(df) {
  
  liquids <- c("iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j")
  liquids_to_check <- intersect(liquids, colnames(df))
  
  df$liquids_all_same <- apply(df[liquids_to_check], 1, function(x) length(unique(x)) == 1)
  
  foods <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  foods_to_check <- intersect(foods, colnames(df))
  
  df$foods_all_same <- apply(df[foods_to_check], 1, function(x) length(unique(x)) == 1)
  df$foods_all_no <- apply(df[,foods_to_check]==2, 1, all)
  df$foods_all_yes <- apply(df[,foods_to_check]==1, 1, all)
  
  df$liquids_all_same <- apply(df[liquids_to_check], 1, function(x) length(unique(x)) == 1)
  df$liquids_all_no <- apply(df[,liquids_to_check]==2, 1, all)
  df$liquids_all_yes <- apply(df[,liquids_to_check]==1, 1, all)
  
  df <- df %>%
    mutate(flag_no_foods = ifelse(foods_all_no == TRUE & iycf_8 > 0 & age_months >5, 1, 0),
           flag_yes_foods = ifelse(foods_all_yes == TRUE, 1, 0),
           flag_all_foods_no_meal = ifelse(foods_all_yes == TRUE & iycf_8 == 0, 1, 0),
           flag_some_foods_no_meal = ifelse(foods_all_no == FALSE & iycf_8 == 0 & age_months > 5, 1, 0),
           flag_yes_liquids = ifelse(liquids_all_yes == TRUE, 1, 0),
           flag_no_anything = ifelse(iycf_4 != 1 & foods_all_no == TRUE & liquids_all_no == TRUE & age_months > 5, 1, 0))
  
  num_flag_no_foods <- sum(df$flag_no_foods, na.rm = TRUE)
  num_flag_yes_foods <- sum(df$flag_yes_foods, na.rm = TRUE)
  num_flag_all_foods_no_meal <- sum(df$flag_all_foods_no_meal, na.rm = TRUE)
  num_flag_some_foods_no_meal <- sum(df$flag_some_foods_no_meal, na.rm = TRUE)
  num_flag_yes_liquids <- sum(df$flag_some_foods_no_meal, na.rm = TRUE)
  num_flag_no_anything <- sum(df$flag_no_anything, na.rm = TRUE)
  
  if(num_flag_no_foods > 0) {cat(paste0("\n IYCF FLAG 1: No foods mentioned consumed yesterday, but more than 0 solid or semi-solid meals consumed for a child 6 months or older. \n", num_flag_no_foods, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators. \n")) }
  if(num_flag_yes_foods > 0) {cat(paste0("\n IYCF FLAG 2: All foods mentioned consumed yesterday. \n", num_flag_yes_foods, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators.\n \n")) }
  if(num_flag_all_foods_no_meal > 0) {cat(paste0("\n IYCF FLAG 3: All foods mentioned consumed yesterday, but 0 solid or semi-solid meals consumed. \n", num_flag_all_foods_no_meal, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators.\n")) }
  if(num_flag_some_foods_no_meal > 0) {cat(paste0("\n IYCF FLAG 4: Some foods mentioned consumed yesterday, but 0 solid or semi-solid meals consumed for a child 6 months or older. \n", num_flag_some_foods_no_meal, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators.\n")) }
  if(num_flag_yes_liquids > 0) {cat(paste0("\n IYCF FLAG 5: All liquids were mentioned consumed yesterday, which is unlikely. \n", num_flag_yes_liquids, " records were flagged. They will not be counted for numerators of EBF indicator, but still for denominators.\n")) }
  if(num_flag_no_anything > 0) {cat(paste0("\n IYCF FLAG 6: No foods or liquids were mentioned consumed yesterday for a child 6 months or older, which is unlikely. \n", num_flag_no_anything, " records were flagged. No indicators are affected, but please check for data quality.\n")) }
  
  
  return(df)
  
}

