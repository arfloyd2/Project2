Project 2: Creating a Vignette for API Interactions
================
Angelice Floyd
2023-10-11

# Introduction and Requirements

## Introduction

This project creates a vignette for reading and summarizing data from a
FEMA ( Federal Emergency Management Agency) API. The first section will
consist of pulling in all of the necessary libraries needed to run the R
program, contact the API from the FEMA webiste, and create the
exploratory data analysis from the data. The second section will create
the functions for interacting with the API. The Third section will use
the data from the API function interaction for some exploratory data
analysis. Finally, there will be a wrap up to summarize what was done in
the vignette.

## Requirements

This section loads the packages required for interacting with the FEMA
API, manipulating the data, and conducting exploratory data analysis.

``` r
library(tidyverse)
library(httr)
library(jsonlite)
```

# Functions for API Interactions

## [Public Assistance Funded Projects Endpoint](https://www.fema.gov/openfema-data-page/public-assistance-funded-project-summaries-v1)

The first function created interacts with the Public Assistance endpoint
of the FEMA API. This data contiains numerical and categorical summary
information on funding projects that FEMA conducted geared towards
public assistance. For consistency and use in later analysis, the two
state abbreviation was created. This function is designed to filter the
data by full state name. Thus, if a string that is not a valid state
name ( or the name is not available) is entered, an error message will
appear prompting you to enter a valid or available state name. One thing
that I found interesting about this data was that a large amount of
participants were southern states. Thus, some of the main states in the
dataset were Texas, Alabama, and Tennessee\> there are some states like
Colorado and North Dakota, but the majority are southern states.

``` r
PAdata <- function(statename = "all") {
  #pull the public assistance summaries from the Public assistance endpoint
  
  outputAPI <- fromJSON(
    "https://www.fema.gov/api/open/v1/PublicAssistanceFundedProjectsSummaries" 
    )
  
   APIData <- outputAPI$PublicAssistanceFundedProjectsSummaries %>% 
     mutate(stateabb = state.abb[match(state,state.name)])
 
   #If the default state name of 'all' is not included, then make sure that the 
   #chosen name is included, and filter, otherwise, print out an error
   if(statename != "all"){
     if(statename %in% APIData$state) {
       output <- APIData %>% filter(state == statename)
     }
     else{
       message <- print("Please try again and enter a valid state name")
       stop(message)
     }
   }
   else{ 
     output <- APIData
   }
  return(output)
}

data <- PAdata()
print(head(data),30)
```

This next set of code modifies the endpoint to filter on pulling data
from either the full state name, the county name of the region, or the
two letter abbreviated state name. Thus, the user could type in state
name and the data filters on the state name value. However, if the user
chooses to type in either the two letter state name or the county name,
the API modifies to filter on the given value. Further, all of this can
be typed in as one string under statename, where the default is all
state values. Finally, the output pulls all of the relevant variables
that will be used in the Exploratory Data analysis.

``` r
PNUMData <- function(statename = "all") {
  
  outputAPI <- fromJSON(
    "https://www.fema.gov/api/open/v1/PublicAssistanceFundedProjectsSummaries")
  APIData <- outputAPI$PublicAssistanceFundedProjectsSummaries %>% 
    mutate(stateabb = state.abb[match(state,state.name)])
  output <- APIData

  if(statename != "all"){
   
    if(statename %in% APIData$state) {
      
      output <- APIData %>% filter(state == statename)
    }
    
    else if( statename %in% APIData$county) {
      
      #Initial URL for the API 
      URL1 <- "https://www.fema.gov/api/open/v1/" 
      
      #Extended endpoint with the county filter
      endpoint <- "PublicAssistanceFundedProjectsSummaries?$filter=county%20eq"
      
      #final URL that will putll the above data 
      finURL <- paste0(URL1, endpoint,paste('%20%27',statename,'%27',sep=""),sep="")
      
      #Object that creates communicates to the FEMA API
      outputAPI <- fromJSON(finURL)
      
      #Finally, these objects create the dataset for the API Data
      output <- outputAPI$PublicAssistanceFundedProjectsSummaries
    }
    
   else if(length(statename) == 1) {
     short <- state.name[match(statename,state.abb)]
     
     #Initial URL for the API 
     URL1 <- "https://www.fema.gov/api/open/v1/" 
     
     #Extended endpoint with the county filter
     endpoint <- "PublicAssistanceFundedProjectsSummaries?$filter=state%20eq"
     
     #final URL that will putll the above data 
     finURL <- paste0(URL1, endpoint,paste('%20%27',short,'%27',sep=""),sep="")
     
     #Object that creates communicates to the FEMA API
     outputAPI <- fromJSON(finURL)
     
     #Finally, these objects create the dataset for the API Data
     output <- outputAPI$PublicAssistanceFundedProjectsSummaries
   }
   
  }
  else{
    
  }
  
  finoutput <- output %>% select(state,county,incidentType,county,educationApplicant,numberOfProjects,federalObligatedAmount)
 
 return(finoutput)
}

data1 <- PNUMData()

print(head(data1), 30)
```

Below is an output example for the county of Lamar in Alabama.

``` r
print(PNUMData("Lamar"))
```

## [Individual Assistance for Homeowners](https://www.fema.gov/openfema-data-page/housing-assistance-program-data-owners-v2)

This next API Endpoint produces financial data from the FEMA Housing
Assistance Program for Homeowners by zip code, county, city, and state
for registered states. Since, I will not be excluding any data for the
numerical analysis, all of the modifications and connections are done in
the same function.The first portion of the code is used to produce an
initial connection to the `HousingAssistanceOwners` API. This brings in
the data for all of the states, counties, zip codes and cities- this is
the default setting for the statename input. If the user chooses not to
incorporate all of the regional based data, there are some options that
they could type in which correspond to the following :  
\* Two letter state abbreviation if the state is registered: ex: AL  
\* specific full state name  
\* Zip code if the region is registered and in the dataset : This has to
be in a character string.  
\* City: must be in all capitals.

``` r
IAData <- function(statename = "all") {
  
  outputAPI <- fromJSON(
  "https://www.fema.gov/api/open/v2/HousingAssistanceOwners" 
  )

  APIData <- outputAPI$HousingAssistanceOwners %>%
    rename("stateabb"=state) %>%
    mutate(state = state.name[match(stateabb,state.abb)])
  output <- APIData

  #If the default state name of 'all is not included, then make sure that the 
  #chosen name is included, and filter, otherwise, print out an error
  if(statename != "all"){
    if(statename %in% APIData$state) {
    output <- APIData %>% filter(state == statename)
    }
  
  else if( statename %in% APIData$zipCode) {
    
    #Initial URL for the API 
    URL1 <- "https://www.fema.gov/api/open/v2/" 
    
    #Extended endpoint with the zip code filter
    endpoint <- "HousingAssistanceOwners?$filter=zipCode%20eq"
    
    #final URL that will putll the above data 
    finURL <- paste0(URL1, endpoint,paste('%20%27',statename,'%27',sep=""),sep="")
    
    #Object that creates communicates to the FEMA API
    outputAPI <- fromJSON(finURL)
    
    #Finally, these objects create the dataset for the API Data
    output <- outputAPI$HousingAssistanceOwners
  }
  else if( statename %in% APIData$city) {
    
    #Initial URL for the API 
    URL1 <- "https://www.fema.gov/api/open/v2/" 
    
    #Extended endpoint with the zip code filter
    endpoint <- "HousingAssistanceOwners?$filter=city%20eq"
    
    #final URL that will putll the above data 
    finURL <- paste0(URL1, endpoint,paste('%20%27',statename,'%27',sep=""),sep="")
    
    #Object that creates communicates to the FEMA API
    outputAPI <- fromJSON(finURL)
    
    #Finally, these objects create the dataset for the API Data
    output <- outputAPI$HousingAssistanceOwners
  }
  
  else if(length(statename) == 1) {
    short <- state.name[match(statename,state.abb)]
    
    #Initial URL for the API 
    URL1 <- "https://www.fema.gov/api/open/v1/" 
    
    #Extended endpoint with the county filter
    endpoint <- "PublicAssistanceFundedProjectsSummaries?$filter=state%20eq"
    
    #final URL that will putll the above data 
    finURL <- paste0(URL1, endpoint,paste('%20%27',short,'%27',sep=""),sep="")
    
    #Object that creates communicates to the FEMA API
    outputAPI <- fromJSON(finURL)
    
    #Finally, these objects create the dataset for the API Data
    output <- outputAPI$PublicAssistanceFundedProjectsSummaries
  }
  
else{
  
}
  } 
  return(output)
}

data3 <- IAData()

print(head(data3), 30)
```

Below is an output example of the data for a zip code in Arkansas

``` r
print(IAData('99776'))
```

## [Emergency Management Performance Grant](https://www.fema.gov/openfema-data-page/emergency-management-performance-grants-v2)

The final interaction with the FEMA API will be connecting to the
Emergency Management Performance Grant endpoint.This produces a dataset
that shows the amount of grant funding provided to regional emergency
management agencies in order to assist them with their emergency
preparedness initiatives during a specific length of time. The
modifications to the API will be the state, and the beginning date of
the project funded, as it will be interesting to be able to filter the
data pertaining to the times where different states conducted their
emergency preparedness projects.

First, this set of code creates a list of possible combinations of state
names and start dates for reference.

``` r
outputAPI <- fromJSON(
  "https://www.fema.gov/api/open/v2/EmergencyManagementPerformanceGrants" 
)

APIDataIN <- outputAPI$EmergencyManagementPerformanceGrants %>%
  mutate(stateabb = state.abb[match(state,state.name)]) %>%  separate(projectStartDate,c("startdate","starttime"),sep = "T",convert=TRUE, remove = FALSE) %>% separate(projectEndDate,c("enddate","endtime"), sep = "T", convert = TRUE, remove = FALSE) %>% unite("index",state,startdate, remove = FALSE)


print(tibble(index = unique(APIDataIN$index)) %>% separate(index,c("state","startdate"),sep="_") %>% mutate(stateabb = state.abb[match(state,state.name)]))
```

Next, this set of code creates a function that pulls the Emergency
Management Performance Grant data from the endpoint. There are some
options for the user.  
\* Enter nothing -`EMPData()`- and all of the data ( unfiltered ) is
pulled from the endpoint.  
\* Enter the state name ( full or abbreviated), but not the start date -
`EMPData(statename = "AL")`, then if the full name is give, the filtered
data from the first bullet is output, but if the abbreviated version is
given, the full name of the abbreviated version is found and the
endpoint is modified to pull from the converted abbreviation and the
data from the corresponding state with all possible dates is produced.  
\* Enter the start date but not the state name
`EMPData(stdate = "2014-10-01")` ,the startdate is first parsed from the
projectStartDate variable ( since this variable includes both the
startdate and a starttime), united with the starttime, and the endpoint
is modified to find the re-combined value within the projectStartDate.  
\* If both the state name and start date are entered
`EMPData(stdate = "2014-10-01",statename = "AL")`, the program follows
the logic of the previous two bullets, and the endpoint is modified to
search for the two values.

``` r
EMPData <- function(statename = "all",stdate = "all") {
  
  outputAPI <- fromJSON(
    "https://www.fema.gov/api/open/v2/EmergencyManagementPerformanceGrants" 
  )
  
  APIDataIN <- outputAPI$EmergencyManagementPerformanceGrants %>%
    mutate(stateabb = state.abb[match(state,state.name)]) %>%  separate(projectStartDate,c("startdate","starttime"),sep = "T",convert=TRUE, remove = FALSE) %>% separate(projectEndDate,c("enddate","endtime"), sep = "T", convert = TRUE, remove = FALSE)
  
  output <- APIDataIN
  
  #If the default state name of 'all is not included, then make sure that the 
  #chosen name is included, and filter, otherwise, print out an error
  if(statename != "all" & stdate == "all"){
    if(statename %in% APIDataIN$state) {
      output <- APIData %>% filter(state == statename)
    }
    
    else if(length(statename) == 1) {
      short <- state.name[match(statename,state.abb)]
      
      #Initial URL for the API 
      URL1 <- "https://www.fema.gov/api/open/v2/" 
      
      #Extended endpoint with the county filter
      endpoint <- "EmergencyManagementPerformanceGrants?$filter=state%20eq"
      
      #final URL that will putll the above data 
      finURL <- paste0(URL1, endpoint,paste('%20%27',short,'%27',sep=""),sep="")
      
      #Object that creates communicates to the FEMA API
      outputAPI <- fromJSON(finURL)
      
      #Finally, these objects create the dataset for the API Data
      output <- outputAPI$EmergencyManagementPerformanceGrants %>%  separate(projectStartDate,c("startdate","starttime"),sep = "T",convert=TRUE, remove = FALSE) %>% separate(projectEndDate,c("enddate","endtime"), sep = "T", convert = TRUE, remove = FALSE)
    }
  } 
  else if(stdate != "all" & statename == "all") {
    dateval <- paste0(stdate,"T",APIDataIN$starttime[1])
    
    #Initial URL for the API 
    URL1 <- "https://www.fema.gov/api/open/v2/" 
    
    #Extended endpoint with the county filter
    endpoint <- "EmergencyManagementPerformanceGrants?$filter=projectStartDate%20eq"
    
    #final URL that will putll the above data 
    finURL <- paste0(URL1, endpoint,paste('%20%27',dateval,'%27', sep=""),sep="")
    
    #Object that creates communicates to the FEMA API
    outputAPI <- fromJSON(finURL)
    
    #Finally, these objects create the dataset for the API Data
    output <- outputAPI$EmergencyManagementPerformanceGrants %>%  separate(projectStartDate,c("startdate","starttime"),sep = "T",convert=TRUE, remove = FALSE) %>% separate(projectEndDate,c("enddate","endtime"), sep = "T", convert = TRUE, remove = FALSE)
    
  }
    else if(stdate != "all" & statename != "all"){
      if(statename %in% APIDataIN$state){
        dateval <- paste0(stdate,"T",APIData$starttime[1])
      
      #Initial URL for the API 
      URL1 <- "https://www.fema.gov/api/open/v2/" 
      
      #Extended endpoint with the county filter
      endpoint <- "EmergencyManagementPerformanceGrants?$filter=projectStartDate%20eq"
      
      #final URL that will putll the above data 
      finURL <- paste0(URL1, endpoint,paste('%20%27',dateval,'%27%20and%20state%20eq%20%27',statename,"%27",sep=""),sep="")
      
      
      #Object that creates communicates to the FEMA API
      outputAPI <- fromJSON(finURL)
      
      #Finally, these objects create the dataset for the API Data
      output <- outputAPI$EmergencyManagementPerformanceGrants %>%  separate(projectStartDate,c("startdate","starttime"),sep = "T",convert=TRUE, remove = FALSE) %>% separate(projectEndDate,c("enddate","endtime"), sep = "T", convert = TRUE, remove = FALSE)
      
    }
  else if(length(statename)==1){
    #Take the start date given value and convert it into the Project start Date format
    
    dateval <- paste0(stdate,"T",APIDataIN$starttime[1])
    long <- state.name[match(statename,state.abb)]
    
    #Initial URL for the API 
    URL1 <- "https://www.fema.gov/api/open/v2/" 
    
    #Extended endpoint with the county filter
    endpoint <- "EmergencyManagementPerformanceGrants?$filter=projectStartDate%20eq"
    
    #final URL that will putll the above data 
    finURL <- paste0(URL1, endpoint,paste('%20%27',dateval,'%27%20and%20state%20eq%20%27',long,"%27",sep=""),sep="")
    
    #Object that creates communicates to the FEMA API
    outputAPI <- fromJSON(finURL)
    
    #Finally, these objects create the dataset for the API Data
    output <- outputAPI$EmergencyManagementPerformanceGrants
  }
}
  return(output)
}

data4 <- EMPData()

print(head(data4),30)
```

Below are examples of the options available to the user for this
function.

``` r
EMPData()
EMPData(statename = "AK")
EMPData(statename = "Alaska")
EMPData(stdate = "2014-07-01")
EMPData(stdate = "2014-07-01",statename = "AK")
EMPData(stdate = "2014-07-01",statename = "Alaska")
```

# Data Exploration

This section will do some basic exploratory data analysis for the data
from each endpoint.

## Public Assistance Funded Projects

Before the EDA is performed, there were some questions that I had. Which
disaster type corresponded with the highest amount of federal obligated
amount of funding? Which distaster type held the higher amount of
frequencies per registered state? Were there more projects conducted for
grants in certain states? Did the level of educational applications (
whether or not the primary reason for the application was for
educational purposes , TRUE = Yes, FALSE = No) correspond to higher or
lower amounts of grant funding (Federal Obligate Amount Variable) ? Is
there a relationship between higher levels of funding and higher numbers
of projects.

### Data pulls

First, I will conduct a pull of the PNUMData where the education
Applicant value is TRUE and the disaster type is a Severe storm or
Flood.

``` r
numdata <- PNUMData()

print(numdata %>% filter(educationApplicant==TRUE & incidentType %in% c('Severe Storm','Flood')))
```

Second, the below code pulls the PNUMData where the Federal Grant Amount
is greater than \$1,000,000 and the Number of projects is less than 10.
I was surprised that there were so many instances were there were very
few numbers of Projects that corresponded to large grants. It would be
interesting to see what those projects consisted of.

``` r
print(numdata %>% filter(federalObligatedAmount > 1000000 & numberOfProjects < 10 ))
```

### Contingency Tables

To explore which disaster type corresponded with the highest frequncies
by state, below will output a contingency table of IncidentType by
state. What I found interesting about this data was that the severe
storms disaster held the highest level of frequencies for each state.
Furtermore, Iowa and the southern states such as Alabama, and Tennessee
were the states with the highest frequencies geared towards funding for
severe storms projects. Maybe there is a climate component that was the
contributing factor.

``` r
table(numdata$state,numdata$incidentType)
```

    ##               
    ##                Flood Severe Ice Storm Severe Storm Tornado
    ##   Alabama          0                0           73       0
    ##   Arkansas         0                0           46       0
    ##   Colorado         0                0           44       0
    ##   Georgia          0                0            7       0
    ##   Illinois         0                0           25       0
    ##   Iowa             0                0           87       0
    ##   Kansas           0                0            0      13
    ##   Louisiana        0                0           59       0
    ##   Mississippi      0               93            0       0
    ##   North Dakota    35                0            0       0
    ##   Oklahoma         0                0            0      78
    ##   Tennessee        0                0          192       0
    ##   Texas          187                0           40      14
    ##   Wyoming          0                7            0       0

The second contingency table explores how the frequency of the levels of
educationApplicants corresponded to the states. As can be seen in the
results, the FALSE level (instances where the application was not filled
out solely for educational purposes) contained the highest amount of
frequencies than the cases where the purpose was solely educatoinal.

``` r
table(numdata$state,numdata$educationApplicant)
```

    ##               
    ##                FALSE TRUE
    ##   Alabama         71    2
    ##   Arkansas        42    4
    ##   Colorado        42    2
    ##   Georgia          6    1
    ##   Illinois        22    3
    ##   Iowa            85    2
    ##   Kansas          11    2
    ##   Louisiana       57    2
    ##   Mississippi     92    1
    ##   North Dakota    34    1
    ##   Oklahoma        62   16
    ##   Tennessee      187    5
    ##   Texas          220   21
    ##   Wyoming          7    0

### Numerical Summaries

To Explore the question for whether a disaster type corresponded to a
larger level of funding, the below code will display the mean, max, and
min values of funding for each storm type. From the results, it can be
seen that the Tornado storm type obtained the highest average funding
out of all of the storm types, it also had the highest maximum amount of
funding, which would make sense seeing that the damage done from a
tornado is highly catastrophic, so larger grant funding to address
issues surrounding that type of disaster would be reasonable.

``` r
print(numdata %>% summarize(MinFunding = min(federalObligatedAmount),MeanFunding = mean(federalObligatedAmount), MaxFunding = max(federalObligatedAmount), .by=incidentType))
```

To explore the question of whether there were more projects conducted
for grants in certain states, below is a table of the minimum, average
and maximum number of projects in each state. Based on the results, even
though Texas has the highest maximum number of projects, North Dakota
held the higest average amount of projects ( though marginally higher
than Texas).

``` r
numsum <- numdata %>% summarize(MinNumProjects = min(numberOfProjects),MeanNumProjects = mean(numberOfProjects), MaxFNumProjects = max(numberOfProjects), .by=state) 

print(numsum %>% arrange(desc(MeanNumProjects)))
```

### Plots

To answer the question for which the education applicant level
corresponded the highest level of funding, below will show a bar graph
of funding per incident for each eaducation level. As can be seen in
each of the disaster types, the educationalApplicant level of False,
where the purpose of the application was not solely for educational
purposes, obtained more funding than the level where the purpose was
solely for education. However, it was interesting to see that the
Tornado incident type had the highest level of funding for educational
applicants out of all of the incident types.

``` r
 ggplot(data=numdata, aes(x=incidentType,y=federalObligatedAmount, fill =educationApplicant )) +
  geom_bar(stat = "identity",position= position_dodge()) + 
  labs(x = "Disaster Type", y= "Amount of Funding", title= "Amount of PA Funding by Disaster Type Colored by educationApplicant level") 
```

![](README_files/figure-gfm/plot1-1.png)<!-- -->

To answer the question of whether there is a relationship between
amounts of funding and numbers of projects, below is a scatter plot.
After separating the results by incident, Flooding seemed to have the
clearest relationship between the number of projects and the funding- in
which it seems like there is a small ( almost negligible) positive
relationship between the number of projects and grant amount. Whereas,
for the other incident types, most of the data is clustered at the lower
end , with a few outliers that are at the middle level and correspond to
higher levels of funding, as can be seen in the Severe Storm plot.

``` r
 ggplot(data= numdata, aes(x=numberOfProjects,y= federalObligatedAmount)) +
  geom_point(position= "identity") + 
  labs(x = "Number of Prjects", y= "Public Assistance Grant Amount", title= "Relationship Between Projects and Funding") +
  facet_wrap(~incidentType)
```

![](README_files/figure-gfm/plot2-1.png)<!-- -->

## Individual Assistance for Homeowners

This data contains information about the number of applicants who had
disaster damage at the levels of no damage, damage between \$1.00 -
\$10,000 , between \$10,001-\$20,000, between \$20,001-\$30,000 and
above \$30,000. Thus, I would be interested to see what are the counts
of applicants for each of those levels at each state. I would also like
to see some summary statistics for the amount of funding for
repair/Replace, rental and other needs grants that were provided and how
that funding was distributed across the states. Third, I would like to
see if there was a relationship between the total amount of FEMA
aplicants who received an inspection and the total damage recorded by
FEMA at the time of the inspection. Finally, It would be interesting to
see the which states had the highest amount of valid registrations.

### Numerical Summaries

To see the frequncies of applicants with disaster damange at the
variious price points, below shows the sum of the fields in each
state/territory. Not surprisingly, Texas had the bulk of the funding,
However, it was interesting to see that the second largest amount of
funding went to Guam, which is a US territory.

``` r
numdata <- IAData() 

print(numdata %>% summarize(NoDamage = sum(noFemaInspectedDamage), Btwn1and10000 = sum(femaInspectedDamageBetween1And10000), Btwn10001and20000 = sum(femaInspectedDamageBetween10001And20000), Btwn20001and30000 = sum(femaInspectedDamageBetween20001And30000),GT30000 = sum(femaInspectedDamageGreaterThan30000), .by=stateabb))
```

Below shows the total, mean and maximum values of repar/replace, rental
and other needs funding for each state. It can be seen that, across the
assistance types, much of the funding went to the sourthern regions. It
is also interesting to see that the majority of these participants for
this housing funding came from southern regions.

``` r
print(numdata %>% summarize(RepairSum = sum(repairReplaceAmount), RepairMean = mean(repairReplaceAmount), RepairMax = max(repairReplaceAmount), RentalSum = sum(rentalAmount), RentalMean = mean(rentalAmount), RentalMax = max(rentalAmount) , otherSum = sum(otherNeedsAmount) , otherMean = mean(otherNeedsAmount), otherMax = max(otherNeedsAmount),  .by=stateabb))
```

### Plots

Next, to see if there was a relationship between total amount of
applicants and total amount of damage recorded, below shows a scatter
plot between the two along with a best fit line of the data.There was a
small positive relationship between the number of inspections and the
amount of damage. Furthermore, the line of best fit seems to have been
impacted by an outlier with a high number of inspections and high amount
of damage.

``` r
ggplot(data = numdata) + 
  geom_point(mapping = aes(x = totalInspected, y = totalDamage)) +
  geom_smooth(mapping = aes(x = totalInspected, y = totalDamage)) +
   labs(x = "Number of Applicants who Received Inspection", y= "Total Damage Recorded", title= "Relationship Between Inspectionsa and Damage") 
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](README_files/figure-gfm/relation-1.png)<!-- -->

Finally, in order to see which state had the highest amount of valid
registrations, below shows a pie chart of the states and their
proportions of numbers of valid registrations.

``` r
#position of labels 
labels <- numdata %>% arrange(desc(stateabb)) %>% summarize(totreg = sum(validRegistrations),.by = stateabb) %>% mutate(prop = totreg / sum(totreg)*100) %>% mutate(ypos = cumsum(prop) - 0.5*prop )

ggplot(labels,aes(x = "", y = prop, fill = stateabb)) + 
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar( "y", start = 0) +
  theme_void() +
  theme(legend.position = "none") + 
  geom_text(aes(y = ypos, label= stateabb ))  + 
  labs(title = "Total Registrations per State")
```

![](README_files/figure-gfm/pie-1.png)<!-- -->

## Emergency Management Performance Grants

This data contains information on funding given to U.S regions for
assistance in preparing for disasters and hazards. The data is broken
out by start date, predicted end date ,and closing year. Seeing that
there are many different start dates, I would be interested to see how
many states were recorded for each start year. Further, it would be
interesting to see how much funding was given out per start year.
Particularly, I would like to see how the funding values are distributed
for the closing year and which states received large amounts of funding
( close to or over \$5 million).

### Numerical Summaries

To see how how many states were recorded per start date, below shows a
contingency table of start date per state. As can be seen, most of the
projects were recorded in 2013 and 2014.

``` r
numdata <- EMPData() %>% separate(startdate,c("Year", "Month", "Day"),sep = "-") 

table(numdata$state,numdata$Year)
```

    ##                       
    ##                        2013 2014 2015 2016
    ##   Alabama                 0    1    0    0
    ##   Alaska                  1   13    0    0
    ##   American Samoa          0    1    0    0
    ##   Arizona                 0    2    0    0
    ##   Arkansas                0   75    1    0
    ##   California              0  109   11    0
    ##   Colorado                0   79    0    0
    ##   District of Columbia    1    0    0    0
    ##   Florida                 2   67   32    0
    ##   Hawaii                  0    5    0    0
    ##   Idaho                  44    1    0    0
    ##   Illinois              119    1    0    0
    ##   Indiana                 5  119   21    0
    ##   Iowa                    2    0    0    0
    ##   Kansas                  1    1    0    0
    ##   Kentucky                1    0    0    0
    ##   Louisiana              38   60   14    0
    ##   Maine                   1    0    0    0
    ##   Maryland               24   22    2    1
    ##   Michigan               95    1    0    0
    ##   Minnesota               0   26    1    0

To see how much funding was given out per start year, below will show
data for the total funding per year. This summary agrees with the above
contingency table where the most funding came from 2013 and 2014.

``` r
numdata %>% summarize(TotalFunds = sum(fundingAmount), .by = Year)
```

### Plots

We could see how much funding was provided per state. Below is a bar
graph that shows how much funding was provided per state for funds
greater than 5,000,000 dollars. From the results, California showed the
most amount of funding at close to \$30,000,000. I would be curious to
know if that funding was for emergencies like forest fires, or other
storm emergencies.

``` r
numdata1 <-  EMPData() %>% 
  summarise(TotalFunds = sum(fundingAmount), .by = state)  %>% filter ( TotalFunds > 5000000)
  
 ggplot(data=numdata1, aes(x=state,y=TotalFunds, )) +
  geom_bar(stat = "identity",position= position_dodge()) + 
  labs(x = "State", y= "Amount of Funding", title= "Amount of Emergency Preparedness Funding by State")
```

![](README_files/figure-gfm/bar1-1.png)<!-- -->

Finally, since there was only one closeout year, I was interested to see
how the data was distributed for smaller amounts of funding.

``` r
numdata2 <- numdata %>% filter(fundingAmount < 1000000)

ggplot(numdata2) + 
  geom_density(mapping=aes(x=fundingAmount, fill = reportingPeriod), position="identity") +
  labs(x = "Amount of Funding", title = "Smoothed Distribution of Funding")
```

![](README_files/figure-gfm/hist-1.png)<!-- -->

# Wrap Up

In summary, I hope that this vignette provides and idea of how to
connect to data from various endopoints of an API and to perform basic
exploratory data analysis. There is a multitude of APIs to work with,
but the FEMA data seemed to pull my interest, as it contained some
important data on federal assistance to US states in preparation of and
response to disasters - especially in light of some of the major natural
disasters or severe weather occurances that have occured over the past
years.
