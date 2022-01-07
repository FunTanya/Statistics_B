######################################################
# Categorical Data                    ################
# Employees of a Small Business Company ##############
EX_DATA_DF <- readRDS("data/EX_DATA_DF.rds")
EX_DATA_DF$Years <- floor(EX_DATA_DF$Months/12)
# bar chart using plot or barplot
plot(EX_DATA_DF$Degree, xlab = "Academic Degree", ylab = "Frequency")
barplot(table(EX_DATA_DF$Degree), xlab = "Academic Degree", ylab = "Frequency")
# Pareto Chart 
qcc::pareto.chart(table(EX_DATA_DF$Degree), main =" Pareto Chart for Degree")
# Grouped  Bar Charts
barplot(table(EX_DATA_DF$Degree,EX_DATA_DF$Gender), beside = TRUE)
# Component Bar Charts
barplot(table(EX_DATA_DF$Degree,EX_DATA_DF$Gender), beside = FALSE)
# Spine Plot
spineplot(table(EX_DATA_DF$Gender,EX_DATA_DF$Degree), main = "Spine Plot for Company Employee Data")
# or Mosaic Plot
mosaicplot(table(EX_DATA_DF$Gender,EX_DATA_DF$Degree), main = "Mosaic Plot for Company Employee Data")
# Pie Chart
pie(table(EX_DATA_DF$Degree))


######################################################
# Numerical Data                      ################
# Employees of a Small Business Company ##############
#Bar chart
plot(table(EX_DATA_DF$Children))
barplot(table(factor(EX_DATA_DF$Children, levels = 0:5)))
#Dot plot
stripchart(EX_DATA_DF$Years, method = "stack", offset = 0.5, pch=19)
# Histogram
hist(EX_DATA_DF$Salary,breaks = 8, main = "Histogram of Employee Salaries", xlab = "Salary")
# Ogive
plot(ecdf(EX_DATA_DF$Salary),verticals = FALSE)
plot(actuar::ogive(EX_DATA_DF$Salary), main = "Ogive for Grouped Data")
# Scatter Plot
plot(EX_DATA_DF$Years,EX_DATA_DF$Salary, xlab = "Duration", ylab = "Salary")