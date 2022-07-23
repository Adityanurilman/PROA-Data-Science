#Membuat Data Frame
AL. <- data.frame(a = c(11, 22, 33), b = c(44, 55, 66), c = c(77, 88, 99)) #membuat variabel yang berisi dataframe
AL.

AL.[[2]] #akses kolom kedua
AL.[[1]][[2]] #akses kolom pertama, baris kedua
AL.[[3]][[3]] #akses kolom ketiga, baris ketiga

#Membuat Vector dan Index Position
A <- c("merah", "jingga", "hitam", "hijau", "biru", "ungu") #membuat variabel yang berisi vektor
L <- A[c(5,3,1)] #akses indeks ke-n
L

#Mengisi Syntax Factor
AL <- factor(c("anggur", "duku", "apel", "panda", "sirsak", "mangga")) #membuat variabel berisi factor
AL

AL[4] <- "anggur" #mengganti isian factor
AL

#Menggunakan Function
#membuat fungsi penambahan angka
tambah_angka <- function(A, L) {
  A + L
}
#Menggunakan fungsi dengan isian angka 4
tambah_angka(4, 4)

#Mengganti Missing Value
#membuat variabel dataset
AL <- c(1, 3, 5, 7, 9, 2, NA, 4, 6, 8, NA)
AL

#membuat fungsi
ganti.mean <- function(i) { #nama fungsi usahakan beda dengan variabel dataset
  i[is.na(i)] <- mean(i, na.rm=TRUE); #cari nilai NA, jika ada ganti dengan nilai mean
  i #simpan nilai
}
#menggunakan fungsi
AL <- ganti.mean(AL)
AL

#Statistik - Visualisasi dengan R
library(readr)

Pohon_AL <- read_csv("https://storage.googleapis.com/dqlab-dataset/trees.csv", show_col_types = FALSE)
Pohon_AL

#menampilkan nama kolom
names(Pohon_AL)

#menampilkan tipe data
str(Pohon_AL)

#mengubah nama kolom
names(Pohon_AL)[1] <- "Diameter"
#membuat kolom baru (diameter.ft)
Pohon_AL$diameter.ft <- Pohon_AL$Diameter*0.08333 #konversi inch ke ft
#menampilkan beberapa baris data
head(Pohon_AL)

#menampilkan hasil Statistik
summary(Pohon_AL)

#mengecek missing value
is.na(Pohon_AL)

#uji normalitas kolom Height
shapiro.test(Pohon_AL$Height)

#uji normalitas kolom Volume
shapiro.test(Pohon_AL$Volume)

#uji normalitas kolom diameter.ft
shapiro.test(Pohon_AL$diameter.ft)

plot(density(Pohon_AL$Volume))

#menggunakan Regresi untuk mencari huubungan antara Volume dengan Tinggi dan Diameter(ft)
lm(formula = Volume ~ Height + diameter.ft, data = Pohon_AL) #perhatikan posisi kolom dalam dataset di rumus Regresi nya

#melihat hubungan antara diameter.ft terhadap volume 
plot(Pohon_AL$diameter.ft, Pohon_AL$Volume)

#melihat hubungan antara height terhadap volume
plot(Pohon_AL$Height, Pohon_AL$Volume)

#Pernyataan Masalah
#Analisa Efek Pemberian Obat Tidur
#set library yang dibutuhkan
library("readr")
library("dplyr")

#membuat variabel dataset
Tidur_AL <- read_csv('https://storage.googleapis.com/dqlab-dataset/sleep.csv', show_col_types = FALSE) 
Tidur_AL

#simpan data dalam 2 data.frame/vector berbeda
group1 <- filter(Tidur_AL, Tidur_AL$group == 1)
group2 <- filter(Tidur_AL, Tidur_AL$group == 2)

#melakukan t-test
t_test <- t.test(group1$extra, group2$extra)
t_test

#cara 2
library("tidyverse")
library(readr)

Bobo_AL <- read_csv('https://storage.googleapis.com/dqlab-dataset/sleep.csv', show_col_types = FALSE)

#langsung t-test tanpa membagi data
t_test <- t.test(extra ~ group, data = Bobo_AL)
t_test

#set library yang digunakan
library("ggplot2")

#membuat boxplot
ggplot(Tidur_AL, aes(x=as.character(group), y=extra, fill=as.character(group))) + geom_boxplot()

#Pembelajaran Mesin dengan R
#set library yang dibutuhkan
library(readr)

#membuat variabel dataset
tagihan.listrik <- read_csv("https://storage.googleapis.com/dqlab-dataset/electric_bill.csv", show_col_types = FALSE)
head(tagihan.listrik, 9)

#membuat Regresi untuk mencari hubungan
lm(amount_paid ~  num_people + housearea, data=tagihan.listrik)

#Training dan Testing
#set library yang dibutuhkan
library(readr)
library(caret)

set.seed(123)

iris.AL <- read_csv("https://storage.googleapis.com/dqlab-dataset/iris.csv", show_col_types = FALSE)
head(iris.AL, 9)

#membagi dataset
trainIndex <- createDataPartition(iris.AL$Species, p=0.8, list=FALSE)
training <- iris.AL[trainIndex, ]
testing <- iris.AL[-trainIndex, ]

dim(training)
dim(testing)
tail(training, 30)
testing

#Model Decision Tree
#set library yang dibutuhkan
library(caret) 
library(rpart)
library(readr)

set.seed(123) #membuat pengacakan konsisten

#membuat variabel dataset
beli.AL <- read_csv("https://storage.googleapis.com/dqlab-dataset/suv_data.csv", show_col_types = FALSE)
head(beli.AL, 9)

#membagi data ke training dan testing
trainIndex <- createDataPartition(beli.AL$Purchased, p=0.8, list=FALSE)
training <- beli.AL[trainIndex, ]
testing <- beli.AL[-trainIndex, ]

#membuat model dengan decision tree
pohon_kep <- rpart(Purchased ~ ., data = training, method="class")
prediksi <- predict(pohon_kep, newdata = testing, type = "class")

#evaluasi performance dengan data testing
testing.beli <- factor(testing$Purchased)

#menampilkan hasil evaluasi model 
hasil.eval <- confusionMatrix(prediksi,testing.beli)
hasil.eval
