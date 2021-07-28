## Author: Drew Rosebush
## Title: RISI Data Analyst Candidate Exercise - Analysis
## Date: July 2021
## Reason: An exercise used to assess one's ability to analyze data using R.
# The intention of this exercise is to assess your ability to think critically
# and apply your technical skill set to an analytics problem designed to reflect
# some of the tasks you are likely to encounter as a Data Analyst at RISI, albeit
# in a simplified manner. This exercise is deliberately vague and no specific
# approach or answer is "right".


## Pull in Libraries
library(dplyr) ## used to aggregate and summarize data
library(tidyr) ##  used to tidy data
library(plotly) ## used to visualize patterns
library(RColorBrewer)  ## Color palettes

## Set directory to where this script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  ## Pull in and run cleaning script
  source("candidate_exercise_rosebush_cleaning.R")

  ## Data
  totals = output[[1]] ## Derived from the Totals row at the end of each age group section
  suicideDF = output[[2]] ## Data expressed at the year, acp_name, & age group [15,80)
  ## Ordering Age Groups
  suicideDF$ageGroup = factor(suicideDF$ageGroup, levels=c("Teens","20's","30's","40's","50's","60's","70's"))
  
  ## Aggregating and Analyzing Totals
  total = output[[1]] %>%
    summarise(Total_Deaths= sum(deaths), Avg_Crude_Rate = Total_Deaths/sum(population)*100000)
    
  
  total_by_year=output[[1]] %>% 
    group_by(Years = year_rangeEnd) %>%
    summarise(Total_Deaths= sum(deaths), Avg_Crude_Rate = Total_Deaths/sum(population)*100000)
    summary(total_by_year$Total_Deaths/100000) ## Summary per 100K people
    ## Simple linear model to generate annual change in suicide rates.
    simpleLM = lm(Avg_Crude_Rate~Years, data = total_by_year)
    simpleLM$coefficients[2] ## Suicide rates are increasing by approximately .23 suicides per 100K people
  ## Are suicides and suicide rates changing in the United States over time? Plots below indicate that both the number and rates of suicides are increasing.
  ## Plotting the Total Deaths and Rates by year  
  ## Setting second y-axis for following chart
  axis2 = list(tickfont = list(color = "red"), overlaying = "y", side = "right",title = "Suicide Rate", showgrid=FALSE)
  ## plot suicides and suicides rates over time
  plot_ly(data=total_by_year) %>% 
    add_bars(x = ~Years, y = ~Total_Deaths, name = "Number of Suicides", type = 'bar') %>% 
    add_lines(x = ~Years, y = ~Avg_Crude_Rate, name = "Crude Rate", yaxis = "y2", type="lines") %>% 
    layout(title = "Suicides Across US: 5 year increments",yaxis = list(title="Count") ,yaxis2 = axis2,
      xaxis = list(title="Years"))

    
  ## What ACP Regions are experiencing the increases? All Regions
  ## Number of suicides and crude_rates by Region
  total_acp_region = output[[1]] %>% 
    group_by(Region = acp_name) %>% 
    summarise(Total_Deaths= sum(deaths), Avg_Crude_Rate = Total_Deaths/sum(population)*100000)
  ## Pie Chart: Total Deaths
  plot_ly(data=total_acp_region, labels=~Region, values=~Total_Deaths, type='pie') %>% 
    layout(title = 'Suicides by ACP Region: 2000-2019',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  ## Bar Chart: Avg. Crude Rate
  plot_ly(data=total_acp_region, x=~Region, y=~Avg_Crude_Rate, name="Avg. Crude Rate", type='bar') %>%
    layout(title = 'Suicide Rate by ACP Region: 2000-2019',
           xaxis = list(title="Regions", labels=TRUE),
           yaxis = list(title="Crude Rate"))
  
  ## Putting data into a wide format for plotting side by side

  wide_data =  totals[,c(1,8,11)] %>% ## Spreading data out
    spread(key = "acp_name", value = deaths)
  wide_data = as.data.frame(lapply(wide_data, as.numeric)) ## Convert all attributes in the data.frame to a numeric
  palette <- c(brewer.pal(12, "Paired"),'#50CB86','#4C74C9','#700961')
  ## Plot side by side bar charts grouped by year
  fig1.0 <- plot_ly(wide_data, x = ~year_rangeEnd, y = ~Big.Cities, name = 'Big Cities', type = 'bar',color = palette[1])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Urban.Suburbs, name = 'Urban Suburbs',color = palette[2])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Exurbs, name = 'Exurbs',color = palette[3])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Rural.Middle.America, name = 'Rural Middle America',color = palette[4])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Graying.America, name = 'Graying America',color = palette[5])
  fig1.0 <- fig1.0 %>% add_trace(y = ~College.Towns, name = 'College Towns',color = palette[6])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Middle.Suburbs, name = 'Middle Suburbs',color = palette[7])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Evangelical.Hubs, name = 'Evangelical Hubs',color = palette[8])
  fig1.0 <- fig1.0 %>% add_trace(y = ~African.American.South, name = 'African American South',color = palette[9])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Military.Posts, name = 'Military Posts', color=palette[10])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Working.Class.Country, name = 'Working Class Country', color=palette[11])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Hispanic.Centers, name = 'Hispanic Centers', color=palette[12])
  fig1.0 <- fig1.0 %>% add_trace(y = ~LDS.Enclaves, name = 'LDS Enclaves', color=palette[13])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Native.American.Lands, name = 'Native American Lands', color=palette[14])
  fig1.0 <- fig1.0 %>% add_trace(y = ~Aging.Farmlands, name = 'Aging Farmlands', color=palette[15])
  fig1.0 <- fig1.0 %>% layout(title = 'ACP Regions impacted by Suicides',
                          xaxis = list(title = "Years",
                                       showgrid = FALSE),
                          yaxis = list(title = "Deaths",
                                       showgrid = FALSE))
  
  fig1.0
  
  
  ## Putting data into a wide format for plotting side by side
  wide_data2 =  totals[,c(3,8,11)] %>% ## Spreading data out
    spread(key = "acp_name", value = crude_rate)
  wide_data2 = as.data.frame(lapply(wide_data2, as.numeric)) ## Convert all attributes in the data.frame to a numeric
  ## Plot side by side bar charts grouped by year
  fig2.0 <- plot_ly(wide_data2, x = ~year_rangeEnd, y = ~Native.American.Lands, name = 'Native American Lands', type = 'bar',color = palette[14])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Graying.America, name = 'Graying America',color = palette[5])
  fig2.0 <- fig2.0 %>% add_trace(y = ~LDS.Enclaves, name = 'LDS Enclaves', color=palette[13])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Working.Class.Country, name = 'Working Class Country', color=palette[11])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Evangelical.Hubs, name = 'Evangelical Hubs',color = palette[8])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Military.Posts, name = 'Military Posts', color=palette[10])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Aging.Farmlands, name = 'Aging Farmlands', color=palette[15])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Rural.Middle.America, name = 'Rural Middle America',color = palette[4])
  fig2.0 <- fig2.0 %>% add_trace(y = ~College.Towns, name = 'College Towns',color = palette[6])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Middle.Suburbs, name = 'Middle Suburbs',color = palette[7])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Exurbs, name = 'Exurbs',color = palette[3])
  fig2.0 <- fig2.0 %>% add_trace(y = ~African.American.South, name = 'African American South',color = palette[9])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Hispanic.Centers, name = 'Hispanic Centers', color=palette[12])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Urban.Suburbs, name = 'Urban Suburbs',color = palette[2])
  fig2.0 <- fig2.0 %>% add_trace(y = ~Big.Cities, name = 'Big Cities', color=palette[1])
  fig2.0 <- fig2.0 %>% layout(title = 'ACP Regions impacted by Suicides',
                              xaxis = list(title = "Years",
                                           showgrid = FALSE),
                              yaxis = list(title = "Crude Suicide Rate",
                                           showgrid = FALSE))
  
  fig2.0

  ## Percent changes by Region crude rates
  wide_data3= totals[,c(3,8,11)] %>% ## looking at change
    group_by(acp_name) %>%
    mutate(pct_change_Rate = (crude_rate/lag(crude_rate)-1)*100)
  wide_data3$pct_change_Rate[is.na(wide_data3$pct_change_Rate)] = 0

  wide_data3 =  wide_data3[,2:4] %>% ## Spreading data out
    spread(key = "acp_name", value = pct_change_Rate)
  wide_data3 = as.data.frame(lapply(wide_data3, as.numeric)) ## Convert all attributes in the data.frame to a numeric
  
  fig3.0 <- plot_ly(wide_data3, x = ~year_rangeEnd, y = ~Native.American.Lands, name = 'Native American Lands', type = 'bar',color = palette[14])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Graying.America, name = 'Graying America',color = palette[5])
  fig3.0 <- fig3.0 %>% add_trace(y = ~LDS.Enclaves, name = 'LDS Enclaves', color=palette[13])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Working.Class.Country, name = 'Working Class Country', color=palette[11])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Evangelical.Hubs, name = 'Evangelical Hubs',color = palette[8])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Military.Posts, name = 'Military Posts', color=palette[10])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Aging.Farmlands, name = 'Aging Farmlands', color=palette[15])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Rural.Middle.America, name = 'Rural Middle America',color = palette[4])
  fig3.0 <- fig3.0 %>% add_trace(y = ~College.Towns, name = 'College Towns',color = palette[6])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Middle.Suburbs, name = 'Middle Suburbs',color = palette[7])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Exurbs, name = 'Exurbs',color = palette[3])
  fig3.0 <- fig3.0 %>% add_trace(y = ~African.American.South, name = 'African American South',color = palette[9])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Hispanic.Centers, name = 'Hispanic Centers', color=palette[12])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Urban.Suburbs, name = 'Urban Suburbs',color = palette[2])
  fig3.0 <- fig3.0 %>% add_trace(y = ~Big.Cities, name = 'Big Cities', color=palette[1])
  fig3.0 <- fig3.0 %>% layout(title = 'ACP Regions impacted by Suicides',
                              xaxis = list(title = "Years",
                                           showgrid = FALSE),
                              yaxis = list(title = "Percent Change in Suicide Rate per Period",
                                           showgrid = FALSE))
  
  fig3.0

## Suicide Rate by Age groups over time. 
  
  ## Grouping data by age and year to examine the changing suicide rate per 5 year increment.
  totals_age = suicideDF %>%
    arrange(desc(ageGroup)) %>%
    group_by(Age.Group=ageGroup,YEAR = year_rangeEnd) %>% 
    summarise(Total_Deaths = sum(deaths), Total_Population = sum(population), Crude_Rate= Total_Deaths/Total_Population*100000, .groups = 'drop') %>%
    mutate(pct_change_Rate = (Crude_Rate/lag(Crude_Rate)-1)*100)
  totals_age$pct_change_Rate_base2004 = 0

  
  wide_data4 =  totals_age[,c(1,2,6)] %>% ## Spreading data out
    spread(key = "Age.Group", value = pct_change_Rate)
  wide_data4[wide_data4$YEAR==2004,2:8] = 0
  ## Plotting percent change by time period using grouped bar charts by time period
  fig4.0 <- plot_ly(wide_data4, x = ~YEAR, y = ~Teens, name='Teens', type = 'bar',color = palette[1])
  fig4.0 <- fig4.0 %>% add_trace(y = ~`20's`, name='20s',color = palette[2])
  fig4.0 <- fig4.0 %>% add_trace(y = ~`30's`, name='30s', color=palette[3])
  fig4.0 <- fig4.0 %>% add_trace(y = ~`40's`, name='40s', color=palette[4])
  fig4.0 <- fig4.0 %>% add_trace(y = ~`50's`, name='50s',color = palette[5])
  fig4.0 <- fig4.0 %>% add_trace(y = ~`60's`, name='60s', color=palette[6])
  fig4.0 <- fig4.0 %>% add_trace(y = ~`70's`, name='70s', color=palette[7])
  fig4.0 <- fig4.0 %>% layout(title = 'Age Groups impacted by Suicides per 5yr increment',
                              xaxis = list(title = "Years",
                                           showgrid = FALSE),
                              yaxis = list(title = "Percent Change in Suicide Rate",
                                           showgrid = FALSE))
  
  fig4.0
  
  ## Percent changes by Region crude rates
  wide_data3= totals[,c(3,8,11)] %>% ## looking at change
    group_by(acp_name) %>%
    mutate(pct_change_Rate = (crude_rate/lag(crude_rate)-1)*100)
  
  ## Ordering stacks
  suicideDF$ageGroup = factor(suicideDF$ageGroup, levels=c("Teens","20's","30's","40's","50's","60's","70's"))
  
  # ## 3D plotting 
  # plot_ly(data=totals_age,x = ~Age.Group, y =~Crude_Rate , z = ~YEAR) %>% 
  #   add_markers(color=~Age.Group) %>% 
  #   layout(xaxis = list(title='Year'),yaxis = list(title = 'Suicide Rate'), title='Suicide Rate by Age Group over time')
  # 
  ## Suicide Rate by Age groups by ACP Regions
  totals_age_acpRegion = suicideDF %>%
    group_by(acpRegion=acp_name, Age.Group=ageGroup) %>%
    summarise(Total_Deaths = sum(deaths), Total_Population = sum(population), Crude_Rate= Total_Deaths/Total_Population*100000,.groups = 'drop') %>% 
    mutate(subgroup_Ranks = order(order(Crude_Rate, decreasing=TRUE))) 

  ## Ordering stacks by Age Group
  totals_age_acpRegion$Age.Group = factor(totals_age_acpRegion$Age.Group, levels=c("Teens","20's","30's","40's","50's","60's","70's"))
  marker_style <- list(line = list(width = .25,color = 'rgb(0, 0, 0)'))
  plot_ly(totals_age_acpRegion, x=~acpRegion, y=~Crude_Rate, color=~Age.Group,marker=marker_style , type = 'bar') %>%
    layout(xaxis = list(title='American Community Populations'),yaxis = list(title = 'Rate'), title='Suicide Rate by Age Group by ACP Region',barmode = 'stack')
  ## Other way around...
  marker_style <- list(line = list(width = .25,color = 'rgb(0, 0, 0)'))  
  plot_ly(totals_age_acpRegion, x=~Age.Group, y=~Crude_Rate, color=~acpRegion, marker=marker_style ,type = 'bar') %>%
    layout(xaxis = list(title='Age Groups'),yaxis = list(title = 'Rate'), title='Suicide Rate by Age Group by ACP Region',barmode = 'stack')

 
  