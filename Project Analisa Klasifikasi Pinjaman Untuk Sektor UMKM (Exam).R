#Membaca Data External
AL.Loan = read.csv("https://storage.googleapis.com/dqlab-dataset/project.csv")

#Inspeksi Data
head(AL.Loan, 9)

#Tampilkan tipe data setiap kolomnya
str(AL.Loan)

#Statistik Deskriptif Data
summary(AL.Loan$OSL)
summary(AL.Loan)

#Menghapus Kolom
#menghapus kolom yang tidak diperlukan
data.guna = AL.Loan[-c(1,2)]

#menampilkan nama kolom
colnames(data.guna)

#Konversi Data
cek.1 <- data.guna[, 8:11]
head(cek.1, 9)

#Pemilihan Data Kategorik
AL.Loan_kategorik = data.guna[,c("KONDISI_USAHA", "KONDISI_JAMINAN", "REKOMENDASI_TINDAK_LANJUT")]
head(AL.Loan_kategorik, 9)
data.guna$REKOMENDASI_TINDAK_LANJUT = as.factor(data.guna$REKOMENDASI_TINDAK_LANJUT)
str(data.guna$REKOMENDASI_TINDAK_LANJUT)

#melihat hubungan KONDISI_USAHA dengan REKOMENDASI_TINDAK_LANJUT
chisq.test(AL.Loan_kategorik$KONDISI_USAHA, AL.Loan_kategorik$REKOMENDASI_TINDAK_LANJUT)

#melihat hubungan KONDISI_JAMINAN dengan REKOMENDASI_TINDAK_LANJUT
chisq.test(AL.Loan_kategorik$KONDISI_JAMINAN, AL.Loan_kategorik$REKOMENDASI_TINDAK_LANJUT)

#Korelasi antar Variabel Data
library("corrplot")
library("ggcorrplot")

M = data_reduce[,8:11]

# Library corrplot
# -- Pearson correlation
par(mfrow=c(1,1))
corrplot(cor(M), type="upper", order="hclust")
corrplot(cor(M), method="square", type="upper")
corrplot(cor(M), method="number", type="lower")

corrplot(cor(M), method="ellipse")

# -- Kendall correlation
par(mfrow=c(1,1))
corrplot(cor(M, method="kendall"), type="upper", order="hclust")
corrplot(cor(M, method="kendall"), method="square", type="upper")
corrplot(cor(M, method="kendall"), method="number", type="lower")
corrplot(cor(M, method="kendall"), method="ellipse")

# Library ggcorrplot
corr = round(cor(M), 1) # Pearson correlation
ggcorrplot(round(cor(M), 1),
             hc.order = TRUE,
             type = "lower",
             lab = TRUE,
             lab_size = 3,
             method="circle",
             colors = c("tomato2", "white", "springgreen3"),
             title="Correlogram of Data Nasabah",
             ggtheme=theme_bw)

#Pemilihan Variabel
colnames(data.guna)

#memilih kolom yang akan diproses
data.pilih =
data.guna[,c("KARAKTER","KONDISI_USAHA","KONDISI_JAMINAN","STATUS","KEWAJIBAN","OSL","KOLEKTIBILITAS","REKOMENDASI_TINDAK_LANJUT")]
data.non.na = na.omit(data.pilih)
head(data.non.na, 9)

#Transformasi Data
data.pilih.baru = data.pilih
data.pilih.baru$KEWAJIBAN = scale(data.pilih.baru$KEWAJIBAN)[, 1]
data.pilih.baru$OSL = scale(data.pilih.baru$OSL)[, 1]
data.pilih.baru$KEWAJIBAN = cut(data.pilih.baru$KEWAJIBAN, breaks = c(-0.354107,5,15,30))
data.pilih.baru$KEWAJIBAN = as.factor(data.pilih.baru$KEWAJIBAN)
data.pilih.baru$OSL = cut(data.pilih.baru$OSL, breaks = c(-0.60383,3,10,15))
data.pilih.baru$OSL = as.factor(data.pilih.baru$OSL)
data.pilih.baru = na.omit(data.pilih.baru)

head(data.pilih.baru, 9)

#Training Data
library("caret")
library("lattice")

index = createDataPartition(data.pilih.baru$REKOMENDASI_TINDAK_LANJUT, p = .95, list = FALSE)
training = data.pilih.baru[index, ]
testing = data.pilih.baru[-index, ]

dim(training)
dim(testing)

#Modelling
train2 = training
# Setting the reference
train2$REKOMENDASI_TINDAK_LANJUT = relevel(train2$REKOMENDASI_TINDAK_LANJUT, ref = "Angsuran Biasa")
# training the model
require(nnet)
# Training the multinomial model
multinom_model = multinom(REKOMENDASI_TINDAK_LANJUT ~ ., data = train2)

# Checking the model
summary(multinom_model)

#converting the coefficients to odds by taking the exponential of the coefficients.
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

# Predicting the values for train dataset
train2$ClassPredicted = predict(multinom_model, newdata = train2, "class")
train_prob = predict(multinom_model, newdata = train2, "probs")
df = train_prob
df$max=apply(df,1, max)
train2$score = df$max
test_prob = predict(multinom_model, newdata = testing, "probs")
df2 = test_prob
df2$max=apply(df2,1, max)

#membuat tabel klasifikasi
tab_train = table(train2$REKOMENDASI_TINDAK_LANJUT, train2$ClassPredicted)
round((sum(diag(tab_train))/sum(tab_train))*100,4)

test$ClassPredicted = predict(multinom_model, newdata = testing, "class")
test$score = df2$max
tab_test = table(test$REKOMENDASI_TINDAK_LANJUT, test$ClassPredicted)
round((sum(diag(tab_test))/sum(tab_test))*100,4)
