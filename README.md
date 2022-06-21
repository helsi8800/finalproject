# finalproject
require("httr")
require("rvest")
library(httr)
library(rvest)

wikipedia_url <- "https://en.wikipedia.org/w/index.php?utm_medium=Exinfluencer&utm_source=Exinfluencer&utm_content=000026UJ&utm_term=10006555&utm_id=NA-SkillsNetwork-Channel-SkillsNetworkCoursesIBMDeveloperSkillsNetworkRP0101ENCoursera23911160-2022-01-01&title=Template%3ACOVID-19_testing_by_country"
get_wiki_covid19_page <- GET(wikipedia_url) 
get_wiki_covid19_page

root_node <- read_html(wikipedia_url)
root_node

table_node <- html_node(root_node, "table")
table_node

data_frame <- html_table(table_node)
data_frame



summary(data_frame)
preprocess_covid_data_frame <- function(data_frame) {
    
shape <- dim(data_frame)


data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
data_frame <- data_frame[1:172, ]
    
data_frame["Ref."] <- NULL
data_frame["Units[b]"] <- NULL

names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
data_frame$country <- as.factor(data_frame$country)
data_frame$date <- as.factor(data_frame$date)
data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    return(data_frame)
}

wiki_data_frame <- preprocess_covid_data_frame(data_frame)
summary(wiki_data_frame)

write.csv(wiki_data_frame, file = "covid.csv", row.names = FALSE)

wd <- getwd()

file_path <- paste(wd, sep="", "/covid.csv")

print(file_path)
file.exists(file_path)

covid_data_frame_csv <- read.csv("covid.csv")
covid_data_frame_csv

covid_data_frame_csv[5:10, c("country", "confirmed")]

total_confirmed_cases_worldwide = sum(covid_data_frame_csv[ ,"confirmed"])
total_confirmed_cases_worldwide

total_tested_cases_worldwide = sum(covid_data_frame_csv[ ,"tested"])
total_tested_cases_worldwide

positive_ratio = total_confirmed_cases_worldwide/total_tested_cases_worldwide 
positive_ratio


country_column = covid_data_frame_csv[,1]  
country_column                            
class(country_column)                      
as.character(country_column)              
sort(country_column)                     
country_reverse_alphabetical = sort(country_column, decreasing = TRUE)    
print(country_reverse_alphabetical)


matches = regexpr("United.+", covid_data_frame_csv[,"country"])
countries_United = regmatches(covid_data_frame_csv[, "country"], matches)
countries_United

Greece_covid_data = wiki_data_frame[62,c("country", "confirmed", "confirmed.population.ratio")]
Greece_covid_data
USA_covid_data = wiki_data_frame[166, c("country", "confirmed", "confirmed.population.ratio")]
USA_covid_data

if ((Greece_covid_data[,3])>(USA_covid_data[,3])){
    print("Greece has a larger confirmed cases to population ratio than the USA.")
} else { print("USA has a larger confirmed cases to population ratio than Greece.")
        }
        
ratio = wiki_data_frame[,"confirmed.population.ratio"]
threshold = function(rating, ratio=0.01) {
    if (rating<threshold) {print("Low risk")
    } else {print("High risk")
           }
}
