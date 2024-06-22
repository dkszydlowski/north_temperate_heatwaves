#### code to deploy the shiny app

install.packages('rsconnect')

library('rsconnect')

rsconnect::setAccountInfo(name='pp5s02-daniel-szydlowski',
                          token='69C88F3591416EF7F91F4D12E41471E9',
                          secret='bVkKlURON33MAsRU0mmgjxq/eHNOaojaD6jo2fHd')


rsconnect::deployApp(appDir = "./R scripts/RLT heatwave scripts/chl shiny app/")



