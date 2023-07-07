library(caret)
library(tidyverse)
library(readxl)
HPIndia_2016 <-read_excel("House Price India.xlsx", sheet = 1)

colnames(HPIndia_2016) <- c("id",
                            "Date",
                            "number_of_badrooms",
                            "number_of_bathrooms",
                            "living_area",
                            "lot_area",
                            "number_of_floors",
                            "waterfront_present",
                            "number_of_views",
                            "condition_of_the_house",
                            "grade_of_the_house",
                            "Area_of_the_house",
                            "Area_of_the_basement",
                            "Built_Year",
                            "Renovation_Year",
                            "Postal_Code",
                            "Lattitude",
                            "Longitude",
                            "living_area_renov",
                            "lot_area_renov",
                            "Number_of_schools_nearby",
                            "Distance_from_the_airport",
                            "Price")


for (i in 1:ncol(HPIndia_2016)) {
  # print(cor(HPIndia_2016[i], HPIndia_2016[23]))
  if(cor(HPIndia_2016[i], HPIndia_2016[23]) > 0.5)
    print(cor(HPIndia_2016[i], HPIndia_2016[23]))
}

#######################################

# train() function
HPIndia <-read_excel("House Price India.xlsx", sheet = 1)
View(HPIndia_2016)
colnames(HPIndia) <- c("id",
                       "Date",
                       "number_of_badrooms",
                       "number_of_bathrooms",
                       "living_area",
                       "lot_area",
                       "number_of_floors",
                       "waterfront_present",
                       "number_of_views",
                       "condition_of_the_house",
                       "grade_of_the_house",
                       "Area_of_the_house",
                       "Area_of_the_basement",
                       "Built_Year",
                       "Renovation_Year",
                       "Postal_Code",
                       "Lattitude",
                       "Longitude",
                       "living_area_renov",
                       "lot_area_renov",
                       "Number_of_schools_nearby",
                       "Distance_from_the_airport",
                       "Price")
n <- nrow(HPIndia)
id <- sample(1:n, size = 0.9*n)

train_data <- HPIndia[id, ]
test_data <- HPIndia[-id, ]

train_data$Price <- log(train_data$Price)
test_data$Price <- log(test_data$Price)

model <- train(Price ~ number_of_badrooms + 
                 number_of_bathrooms + 
                 living_area + 
                 number_of_floors + 
                 waterfront_present + 
                 number_of_views + 
                 grade_of_the_house + 
                 Area_of_the_house + 
                 Renovation_Year +
                 living_area_renov, 
               data = train_data,
               method = "lm")
model

p <- predict(model,newdata = test_data)

error <- test_data$Price - p
sqrt(mean(error**2))
