Here is some sample code for my 340B project. 

library(lubridate)
library(tidyverse)

df1 <- read_csv("cleaned_hrsa_with_npi.csv")

df2 <- df1 %>%
    filter(specialty_mail_flag != 1 & in_house_flag != 1)

df11 <- df2 %>%
    filter(!is.na(npi)) %>%
    mutate(start = as.Date(contract_begin_date)) %>%
    mutate(end = as.Date(contract_termination_date)) %>%
    mutate(end = if_else(is.na(end), as.Date("2022-12-01"), end)) %>%
    distinct(ce_id, npi, pharmacy_name, pharm_id, pharmacy_state, start, end) %>%
    mutate(start_year = year(start)) %>%
    mutate(end_year = year(end)) %>%
    filter(between(start_year, 2009, 2022))

df12 <- df11 %>%
  mutate(n_2006 = if_else(start <= as.Date("2006-12-01") & as.Date("2006-12-01") <= end, 1, 0),
         n_2007 = if_else(start <= as.Date("2007-12-01") & as.Date("2007-12-01") <= end, 1, 0),
         n_2008 = if_else(start <= as.Date("2008-12-01") & as.Date("2008-12-01") <= end, 1, 0),
         n_2009 = if_else(start <= as.Date("2009-12-01") & as.Date("2009-12-01") <= end, 1, 0),
         n_2010 = if_else(start <= as.Date("2010-12-01") & as.Date("2010-12-01") <= end, 1, 0),
         n_2011 = if_else(start <= as.Date("2011-12-01") & as.Date("2011-12-01") <= end, 1, 0),
         n_2012 = if_else(start <= as.Date("2012-12-01") & as.Date("2012-12-01") <= end, 1, 0),
         n_2013 = if_else(start <= as.Date("2013-12-01") & as.Date("2013-12-01") <= end, 1, 0),
         n_2014 = if_else(start <= as.Date("2014-12-01") & as.Date("2014-12-01") <= end, 1, 0),
         n_2015 = if_else(start <= as.Date("2015-12-01") & as.Date("2015-12-01") <= end, 1, 0),
         n_2016 = if_else(start <= as.Date("2016-12-01") & as.Date("2016-12-01") <= end, 1, 0),
         n_2017 = if_else(start <= as.Date("2017-12-01") & as.Date("2017-12-01") <= end, 1, 0),
         n_2018 = if_else(start <= as.Date("2018-12-01") & as.Date("2018-12-01") <= end, 1, 0),
         n_2019 = if_else(start <= as.Date("2019-12-01") & as.Date("2019-12-01") <= end, 1, 0),
         n_2020 = if_else(start <= as.Date("2020-12-01") & as.Date("2020-12-01") <= end, 1, 0),
         n_2021 = if_else(start <= as.Date("2021-12-01") & as.Date("2021-12-01") <= end, 1, 0)) %>%
  ## Groups the data by the column "pharmacy_name" and "pharm_id"
  group_by(npi, pharmacy_name,
           pharm_id) %>%
  ## Selects all the contract for ecah year to the columns from n_2006 to n_2021.
  summarise_at(vars(c(n_2006:n_2021)), ~sum(.)) %>%
  ungroup()

df13 <- df12 %>%
  select(-pharmacy_name, -pharm_id) %>%
  mutate(npi = as.character(npi)) %>%
  gather(year, n_contracts, -npi) %>% 
  mutate(year = gsub("n_", "", year),
         contract_bucket = case_when(n_contracts == 0 ~ "0",
                                    n_contracts == 1 ~ "1",
                                    n_contracts == 2 ~ "2",
                                    n_contracts >= 3 & n_contracts <= 5 ~ "3-5",
                                     n_contracts > 10 ~ ">10",
                                     n_contracts > 5 ~ ">5",
                                     T ~ "0"))

df15 <- df13 %>% group_by(year, contract_bucket) %>%
    summarize(count = n())

df15 <- df15 %>% group_by(year, contract_bucket) %>%
    summarize(n = sum(count)) %>%
    mutate(percentage = ((n/sum(n))*100))

df15$year = as.numeric(df15$year)

df15 <- df15 %>%
  filter(between(year, 2009, 2022))

write.csv(df15, "Figure 1 Share Data.csv", row.names = FALSE)

df16 <- df15 %>%
  filter(contract_bucket != 0)

df16$contract_bucket <- as.character(df16$contract_bucket)
df16$percentage <- as.numeric(df16$percentage)
df16$year <- as.numeric(df16$year)


## Sorting the legend accordingly
df16$contract_bucket <- ifelse(df16$contract_bucket == "1", ">1", df16$contract_bucket)
df16$contract_bucket <- ifelse(df16$contract_bucket == "2", ">2", df16$contract_bucket)
df16$contract_bucket <- ifelse(df16$contract_bucket == "3-5", ">3-5", df16$contract_bucket)
df16$contract_bucket <- ifelse(df16$contract_bucket == ">10", "10", df16$contract_bucket)

ggplot(df16, aes(x=year, y=percentage, fill = contract_bucket)) +
geom_area(show.legend = FALSE) +
theme(
plot.title = element_text(size=13, hjust = 0.5),
plot.caption = element_text(size = 13, hjust = 0),
axis.title.x = element_text(size =13, hjust = 0.5),
axis.title.y = element_text(size = 11, angle = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
)+
xlab("") +
labs(x= "Year",
y= "Number of Contract 
per Pharmacy", fill = "")+ 
ylim(0,100) +
guides(fill = guide_legend(byrow = TRUE))+ scale_fill_grey(start = 0, end = 1, aesthetics = "fill") + 
annotate("text", x = 2022, y = 60, label = "1") +
annotate("text", x = 2022, y = 40, label = "2") +
annotate("text", x = 2022, y = 18, label = "3-5") +
annotate("text", x = 2022, y = 8, label = ">5") +
annotate("text", x = 2022, y = 2, label = ">10")

ggsave("Figure 1.png", height = 9, width = 16)





![Figure_1_revisit](https://user-images.githubusercontent.com/27037723/218240351-8e0f9c0f-4af5-4295-95a1-83f50c4b70b6.png)


#########################################
########################################
########################################
library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
library(igraph)

df<- read_csv("cleaned_hrsa_with_npi.csv")

df$start_year<-format(df$contract_begin_date,"%Y")
df$end_year<-format(df$contract_termination_date,"%Y")
## Replace NA in end Year with 2022, because it mean it still on
df<-df %>% mutate_at(vars("end_year"), ~replace_na(.,"2022"))
df2 <- df %>%
filter(!is.na(npi)) %>%
filter(specialty_mail_flag != 1 & in_house_flag != 1) %>%
distinct(ce_id, npi, pharmacy_name, pharm_id, pharmacy_state, start_year, end_year) #%>%
#filter(between(start_year, 2009, 2022))
df2<-arrange(df2,pharmacy_state,start_year)
##
Start_contract_number<-df2%>%
group_by(start_year,npi,ce_id,pharmacy_state)%>%
summarise(n_Start=n())

End_contract_number<-df2%>%
group_by(end_year,npi,ce_id,pharmacy_state)%>%
summarise(n_End=n())
## ‘summarise()‘ has grouped output by ’end_year’, ’npi’, ’ce_id’. You can
## override using the ‘.groups‘ argument.
df3<-full_join(Start_contract_number,End_contract_number,by=c("npi","ce_id","pharmacy_state"))
sum(duplicated(df3))
## [1] 0
df4<-filter(df3,end_year==2022) #I SET year end 2022
df4<-arrange(df4,npi,ce_id)
df5<-df4%>%
group_by(npi,pharmacy_state,ce_id)%>%
mutate(contract.ce_id=n())
##MINESOTA STATE
MN_df4<-filter(df5,
pharmacy_state=="MN",
#n_End!=1,
#n_Start!=1
)
MN_df41 <- data.frame(MN_df4$npi,MN_df4$ce_id)
MN_graph <- graph.data.frame(MN_df41, directed = F)
##Graph network
## NPI IN MINESOTA
MN_NPI<-data.frame(MN_df4$npi,MN_df4$pharmacy_state)
NPI_Graph<-graph.data.frame(MN_NPI,directed = F)
set.seed(1233)

Grouping the Dataframe into communities
The dataframe “MN_graph” was grouped into communities using “edge.betweenness.community()”
x <- edge.betweenness.community(MN_graph)
## Checking the number of communities
length(x)
## [1] 50
Visualizing the Graph
plot(x, MN_graph,
vertex.size=.5,
edge.arrow.size=.3,
vertex.label.cex=.01,)

![MN community CE and NPI](https://user-images.githubusercontent.com/27037723/218240554-d23e1482-9dd0-4cd2-9bc7-4d3927565ad8.png)



