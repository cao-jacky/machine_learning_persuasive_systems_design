
print("demographics-synthetic")

# Function to create the data:
# Please inspect each function seperately and mae sure you understand what it does :)
createData <- function(n, seed=10){
	set.seed(seed)
	
	# Create
	gender <- rbinom(n, 1, .48) 
	height <- round(170 + gender*10 + rnorm(n, 0, 15),1)
	weight <- height / 2 + runif(n,-10,10)
	voting <- rbinom(n, 5, c(.3,.3,.2,.195,.005))
	age <- round(23 + sqrt(rnorm(n, 0, 5)^2),1)
	
	# Recode
	voting <- ifelse(voting==4, 99, voting)
	gender <- ifelse(gender==1, "Male","Female")
	
	# Outlier example
	age[round(2*n/3,1)] <- 622

	# Return data frame
	data <- data.frame(
		"Gender" = gender,
		"Age" = age,
		"Weight" = weight,
		"Height" = height,
		"Voting" = voting)
	
	return(data)
}

# Create the data and store it:
n <- 100
data <- createData(n)
write.csv(data, file="Lecture1_Data.csv")



# Open the data set:
data <- read.csv("Lecture1_Data.csv")

# Inspecting the data
head(data)
summary(data)
dim(data)
?summary

# Factors:
data$Voting <- as.factor(data$Voting)
summary(data$Voting)

# Frequency tables:
table1 <- table(data$Voting)
table1 <- transform(table1, cumFreq = cumsum(Freq), relative = prop.table(Freq))
table1

bins <- 5
HeightBin <- factor(cut(data$Height, breaks=bins))
table2 <- table(HeightBin)
table2 <- transform(table2, cumFreq = cumsum(Freq), relative = prop.table(Freq))
table2

## Linear regression:
mod <- lm(Weight ~ Height, data=data)
summary(mod)
# plot(mod)
pred <- predict(mod)
plot(data$Weight, pred)
cor(data$Weight, pred)

## Note on installing packages:
# Tools -> Install packages
