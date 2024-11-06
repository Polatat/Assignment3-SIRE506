# select table rs422485 
selected_columns <- c("rs4244285", "CYP2C19*2")
new_rs4244285 <- data[, selected_columns]


#select table rs49869893
selected_columns <-c ("rs4986893","CYP2C19*3")
new_rs4986893 <- data[,selected_columns]

#select table rs662
selected_columns <-c ("rs662","PON1.192Q>R")
new_rs662 <- data[,selected_columns]