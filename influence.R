# influence.R

library(dplyr) # need to install with install.packages("dplyr") if not already installed (just need to do first time)

data1 <- data.frame(nominator = c(2, 1, 3, 1, 2, 6, 3, 5, 6, 4, 3, 4), 
                    nominee = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6), 
                    relate = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

data2 <- data.frame(nominee = c(1, 2, 3, 4, 5, 6), 
                    yvar1 = c(2.4, 2.6, 1.1, -0.5, -3, -1))

data3 <- data.frame(nominator = c(1, 2, 3, 4, 5, 6),
                    yvar2 = c(2, 2, 1, -0.5, -2, -0.5))

# merge data1 and data2
# note: we want the nominee's indegree because this is who the nominator is being exposed to

data <- left_join(data1, data2, by = "nominee")
data$nominee <- as.character(data$nominee) # this makes merging later easier

# calculate indegree in tempdata and merge with data
tempdata <- data.frame(table(data$nominee))
names(tempdata) <- c("nominee", "indegree") # rename the column "nominee"
tempdata$nominee <- as.character(tempdata$nominee) # makes nominee a character data type, instead of a factor, which can cause problems
data <- left_join(data, tempdata, by = "nominee")

# Calculating exposure and an exposure term that uses indegree, exposure_plus
data$exposure <- data$relate * data$yvar1
data$exposure_plus <- data$exposure * (data$indegree + 1)

# Calculating mean exposure
mean_exposure <-
    data %>%
    group_by(nominator) %>%
    summarize(exposure_mean = mean(exposure))

mean_exposure_plus <-
    data %>%
    group_by(nominator) %>%
    summarize(exposure_plus_mean = mean(exposure_plus))

# need a final data set with mean_exposure, mean_exposure_plus, degree, yvar1, and yvar2 added

mean_exposure_terms <- dplyr::left_join(mean_exposure, mean_exposure_plus, by = "nominator")

names(data2) <- c("nominator", "yvar1") # rename nominee as nominator to merge these
final_data <- dplyr::left_join(mean_exposure_terms, data2, by = "nominator")
final_data <- dplyr::left_join(final_data, data3, by = "nominator") # data3 already has nominator, so no need to change

# regression (linear models)

model1 <- lm(yvar2 ~ yvar1 + exposure_mean, data = final_data)
summary(model1)

model2 <- lm(yvar2 ~ yvar1 + exposure_plus_mean, data = final_data)
summary(model2)