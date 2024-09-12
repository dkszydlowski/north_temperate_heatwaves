# count lines of code


# 
# ##### see how many lines of code heatwaves is #####
# 
# script.files = list.files("./R scripts/RLT heatwave scripts/")
# script.files = script.files[!grepl("chl shiny app", script.files)]
# script.files = script.files[!grepl("bar chart shiny app", script.files)]
# script.files = script.files[!grepl("timing shiny app", script.files)]
# script.files = script.files[!grepl("troubleshooting 2024", script.files)]
# 
# total = 0
# 
# for(i in 1:length(script.files)){
#   
#   test = readLines(paste("./R scripts/RLT heatwave scripts/", script.files[i], sep = ""))
#   print(script.files[i])
#   
#   total = total + length(test)
#   print(total)
#   
# }
