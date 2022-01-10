## code to prepare `DATASET` dataset goes here

data01 <- read.csv("data-raw/data01.csv", header = F)
colnames(data01)[3] <- "class"
data01$class <- factor(data01$class + 1)

data02 <- read.csv("data-raw/data02.csv", header = F)
colnames(data02)[3] <- "class"
data02$class <- factor(data02$class + 1)

data03 <- read.csv("data-raw/data03.csv", header = F)
colnames(data03)[3] <- "class"
data03$class <- factor(data03$class + 1)

data04 <- read.csv("data-raw/data04.csv", header = F)
colnames(data04)[3] <- "class"
data04$class <- factor(data04$class)

data05 <- read.csv("data-raw/data05.csv", header = F)
colnames(data05)[3] <- "class"
data05$class <- factor(data05$class)

data06 <- read.csv("data-raw/data06.csv", header = F)
colnames(data06)[3] <- "class"
data06$class <- factor(stringr::str_remove(data06$class, "Class "))

data07 <- read.csv("data-raw/data07.csv", header = F)
colnames(data07)[3] <- "class"
data07$class <- factor(data07$class + 1)

data08 <- read.csv("data-raw/data08.csv", header = F)
colnames(data08)[3] <- "class"
data08$class <- factor(data08$class + 1)

data09 <- read.csv("data-raw/data09.csv", header = F)
colnames(data09)[3] <- "class"
data09$class <- factor(data09$class)

data10 <- read.csv("data-raw/data10.csv", header = F)
colnames(data10)[3] <- "class"
data10$class <- factor(data10$class)

data11 <- read.csv("data-raw/data11.csv", header = F)
colnames(data11)[3] <- "class"
data11$class <- factor(data11$class + 1)

data12 <- read.csv("data-raw/data12.csv", header = F)
colnames(data12)[3] <- "class"
data12$class <- factor(data12$class + 1)

data13 <- read.csv("data-raw/data13.csv", header = F)
colnames(data13)[3] <- "class"
data13$class <- factor(data13$class + 1)

data14 <- read.csv("data-raw/data14.csv", header = F)
colnames(data14)[3] <- "class"
data14$class <- factor(data14$class + 1)

data15 <- read.csv("data-raw/data15.csv", header = F)
colnames(data15)[3] <- "class"
data15$class <- factor(data15$class + 1)

data16 <- read.csv("data-raw/data16.csv", header = F)
colnames(data16)[3] <- "class"
data16$class <- factor(data16$class)

usethis::use_data(data01, overwrite = TRUE)
usethis::use_data(data02, overwrite = TRUE)
usethis::use_data(data03, overwrite = TRUE)
usethis::use_data(data04, overwrite = TRUE)
usethis::use_data(data05, overwrite = TRUE)
usethis::use_data(data06, overwrite = TRUE)
usethis::use_data(data07, overwrite = TRUE)
usethis::use_data(data08, overwrite = TRUE)
usethis::use_data(data09, overwrite = TRUE)
usethis::use_data(data10, overwrite = TRUE)
usethis::use_data(data11, overwrite = TRUE)
usethis::use_data(data12, overwrite = TRUE)
usethis::use_data(data13, overwrite = TRUE)
usethis::use_data(data14, overwrite = TRUE)
usethis::use_data(data15, overwrite = TRUE)
usethis::use_data(data16, overwrite = TRUE)
