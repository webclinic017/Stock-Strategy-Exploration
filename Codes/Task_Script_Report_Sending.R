## Loading Packages
library('tidyverse')
library('rmarkdown')
library('randomForest')
library('stringr')

## Defining Pandoc System Location
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

## Running Reports
if(str_detect(weekdays(Sys.Date()),"Tuesday|Friday")){
  render(input = '//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Codes/Automated_Summary.RMD',
         output_file = paste0('//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Reports/Algorithim_Summary_',Sys.Date(),'.html'))
}
render(input = '//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Codes/Automated_Summary.RMD',
       params = list(Access_LVL = "internal"))

## Renaming and Moving Internal Report
file.copy('//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Codes/Automated_Summary.html',
          '//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Reports/')
file.remove('//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Codes/Automated_Summary.html')
file.rename('//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Reports/Automated_Summary.html',
            paste0('//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/ProAct Transport/Initial Field Trial/Criteria Summary/Reports/Algo_Summary_INT_',Sys.Date(),'.html'))

