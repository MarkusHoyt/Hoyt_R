library(readr)
library(dplyr)
library(countrycode)
library(tidyr)
library(ggplot2)
library(tidyverse)
# uploading data.csv

data <- read_csv("C:/Users/markt/DSE5002-1-main/DSE5002-1-main/R Project/data.csv")
View(data)

#renaming column 1
colnames(data)[colnames(data) == "...1"] <- "Row_ID"

#next I will clean the data 
# unabreviating experience level

data$experience_level[data$experience_level == 'EN'] <- 'Entry Level'
data$experience_level[data$experience_level == 'MI'] <- 'Mid Level'
data$experience_level[data$experience_level == 'SE'] <- 'Senior Level'
data$experience_level[data$experience_level == 'EX'] <- 'Executive Level'
data

# unabreviating employment type

data$employment_type[data$employment_type == 'PT'] <- 'Part Time'
data$employment_type[data$employment_type == 'FT'] <- 'Full Time'
data$employment_type[data$employment_type == 'CT'] <- 'Contract Work'
data$employment_type[data$employment_type == 'FL'] <- 'Freelance'
data

#New Jersey?
data$employee_residence[data$employee_residence == 'Jersey'] <- 'United States'

# combining salary with the countrys currency
data <- data %>%
  unite('Salary_currency', salary:salary_currency, sep = " ", remove = FALSE)

#getting rid of uneeded columns 
data = select(data, -7, -8)

#converting ISO 3166 to country name
data$employee_residence <- countrycode(c(data$employee_residence), origin = 'genc2c', destination = 'country.name')
data$company_location <- countrycode(c(data$company_location), origin = 'genc2c', destination = 'country.name')

# changing company size column 
data$company_size[data$company_size == 'S'] <- 'Small (>50)'
data$company_size[data$company_size == 'M'] <- 'Medium (50-250)'
data$company_size[data$company_size == 'L'] <- 'Large (250<)'
data

#average entry level pay for full time US
us_pay <- data %>% 
  filter(employee_residence =='United States')
entrylvl <- us_pay %>% 
  filter(experience_level =='Entry Level')
entrylvl <- entrylvl %>% 
  filter(employment_type =='Full Time')
entryavg <- entrylvl%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(entryavg,aes(x=work_year, y=name, fill=employee_residence)) +
  geom_col(stat="identity") +
  labs(x='year',
       y='avg. salary',
       title='Average Entry Level salary in the US') +
  scale_y_continuous(breaks=c(0, 50000, 75000, 100000, 125000, 150000))

#average mid level pay for full time US
us_pay <- data %>% 
  filter(employee_residence =='United States')
midlvl <- us_pay %>% 
  filter(experience_level =='Mid Level')
midlvl <- midlvl %>% 
  filter(employment_type =='Full Time')
midavg <- midlvl%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(midavg,aes(x=work_year, y=name, fill=employee_residence)) +
  geom_col(stat="identity") +
  labs(x='year',
       y='avg. salary',
       title='Average Mid Level salary in the US') +
  scale_y_continuous(breaks=c(0, 50000, 75000, 100000, 125000, 150000))

#average senior level pay for full time US
us_pay <- data %>% 
  filter(employee_residence =='United States')
seniorlvl <- us_pay %>% 
  filter(experience_level =='Senior Level')
seniorlvl <- seniorlvl %>% 
  filter(employment_type =='Full Time')
senioravg <- seniorlvl%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(senioravg,aes(x=work_year, y=name, fill=employee_residence)) +
  geom_col(stat="identity") +
  labs(x='year',
       y='avg. salary',
       title='Average Senior Level salary in the US') +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000))

#average senior level pay for full time US
us_pay <- data %>% 
  filter(employee_residence =='United States')
execlvl <- us_pay %>% 
  filter(experience_level =='Executive Level')
execlvl <- execlvl %>% 
  filter(employment_type =='Full Time')
execavg <- execlvl%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(execavg,aes(x=work_year, y=name, fill=employee_residence)) +
  geom_col(stat="identity") +
  labs(x='year',
       y='avg. salary',
       title='Average Exec Level salary in the US') +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000))

#creating a dataframe of non US jobs
non_us<-data[!(data$employee_residence=="United States"),]
non_us

#average entry level pay for full time Non-US
entryoconus <- non_us %>% 
  filter(experience_level =='Entry Level')
entryoconus <- entryoconus %>% 
  filter(employment_type =='Full Time')
enavgoconus <- entryoconus%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(enavgoconus,aes(x=employee_residence, y=name, fill=work_year)) +
  geom_col() +
  labs(x='Country',
       y='avg. salary',
       title='Average entry Level salary in the Non-US') + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(breaks=c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000))

#average Mid level pay for full time Non-US
midoconus <- non_us %>% 
  filter(experience_level =='Mid Level')
midoconus <- midoconus %>% 
  filter(employment_type =='Full Time')
midavgoconus <- midoconus%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(midavgoconus,aes(x=employee_residence, y=name, fill=work_year)) +
  geom_col() +
  labs(x='Country',
       y='avg. salary',
       title='Average Mid Level salary in the Non-US') + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks=c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000))


#average Senior level pay for full time Non-US
senoconus <- non_us %>% 
  filter(experience_level =='Senior Level')
senoconus <- senoconus %>% 
  filter(employment_type =='Full Time')
senavgoconus <- senoconus%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(senavgoconus,aes(x=employee_residence, y=name, fill=work_year)) +
  geom_col() +
  labs(x='Country',
       y='avg. salary',
       title='Average Senior Level salary in the Non-US') + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000))

#average Exec level pay for full time Non-US
execoconus <- non_us %>% 
  filter(experience_level =='Executive Level')
execoconus <- execoconus %>% 
  filter(employment_type =='Full Time')
execavgoconus <- execoconus%>%                                        
  group_by(work_year, employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

ggplot(execavgoconus,aes(x=employee_residence, y=name, fill=work_year)) +
  geom_col() +
  labs(x='Country',
       y='avg. salary',
       title='Average Executive Level salary in the Non-US') + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 225000))

#Finding the average of US Head of data
hodus <- data %>% 
  filter(employee_residence =='United States')
hodus <- hodus %>% 
  filter(job_title == 'Head of Data Science' | job_title == 'Head of Data')
hodusavg <- hodus %>%                                        
  group_by(employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

#Finding the average of Non-US Head of data
hodn <- non_us %>% 
  filter(job_title == 'Head of Data Science' | job_title == 'Head of Data' | job_title == 'Head of Machine Learning')
hodnavg <- hodn %>%                                        
  group_by(Salary_currency) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

#Finding the average of US Data Analyst / engineer
daeus <- data %>% 
  filter(employee_residence =='United States')
daeus <- daeus %>% 
  filter(job_title == 'Data Analyst' | job_title == 'Data Engineer')
daeavg <- daeus %>%                                        
  group_by(employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

#Finding the average of Non-US Data Analyst / engineer
daen <- non_us %>% 
  filter(job_title == 'Data Analyst' | job_title == 'Data Engineer')
daenavg <- daen %>%                                        
  group_by(employee_residence) %>%                        
  summarise_at(vars(salary_in_usd),
               list(name = mean))

#Head of Data spread 
hod <- data %>% 
  filter(job_title == 'Head of Data Science' | job_title == 'Head of Data')

ggplot(hod) +
  geom_point(aes(x=experience_level ,y=salary_in_usd,color=employee_residence)) +
  geom_smooth(method='lm',aes(y=salary_in_usd ,x=experience_level)) +
  facet_grid(.~work_year) +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title='Head of Data avg. salary')

# Data Anayst spread 
hae <- data %>% 
  filter(job_title == 'Data Analyst' | job_title == 'Data Engineer')

ggplot(hae) +
  geom_point(aes(x=experience_level ,y=salary_in_usd,color=employee_residence)) +
  geom_smooth(method='lm',aes(y=salary_in_usd ,x=experience_level)) +
  facet_grid(.~work_year) +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title='Data Analyst/Engineer avg. salary')
