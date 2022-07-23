library(ggplot2)

#[Customer Segmentation]

  #Persiapan Data
#Membaca data csv dan dimasukkan ke variable pelanggan
pelanggan <- read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt",sep="\t")

#Buat variable field_yang_digunakan dengan isi berupa vector "Jenis.Kelamin", "Umur" dan "Profesi"
field_yang_digunakan <- c("Jenis.Kelamin","Umur","Profesi")

#Tampilan data pelanggan dengan nama kolom sesuai isi vector field_yang_digunakan
pelanggan[field_yang_digunakan]

#Konversi data menjadi numerik
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin","Profesi","Tipe.Residen")])

#Penggabungan data
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#Normalisasi Nilai
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun / 1000000

  #Clustering dan Algoritma K-Means
#Bagian K-Means
set.seed(100)
#fungsi kmeans untuk membentuk 5 cluster dengan 25 skenario random dan simpan ke dalam variable segmentasi
segmentasi <- kmeans(x=pelanggan[field_yang_digunakan], center=5, nstart=25)

#Penggabungan hasil cluster
segmentasi$cluster
pelanggan$cluster <- segmentasi$cluster

#Analisa hasil
#Filter cluster ke-1
which(pelanggan$cluster == 1)
length(which(pelanggan$cluster == 2))

#Melihat data cluster ke 3-5
pelanggan[which(pelanggan$cluster == 3),]
pelanggan[which(pelanggan$cluster == 4),]
pelanggan[which(pelanggan$cluster == 5),]

#Melihat cluster means dari objek 
segmentasi$centers

#Membandingkan dengan 2 cluster kmeans, masing-masing 2 dan 5
set.seed(100)
kmeans(x=pelanggan[field_yang_digunakan], centers=2, nstart=25)
set.seed(100)
kmeans(x=pelanggan[field_yang_digunakan], centers=5, nstart=25)

#Bagian K-Means
set.seed(100)
segmentasi <- kmeans(x=pelanggan[field_yang_digunakan], centers=5, nstart=25)
segmentasi$withinss
segmentasi$cluster
segmentasi$tot.withinss

  #Menentukan Jumlah Cluster Terbaik
#Bagian Data Preparation
pelanggan <- read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt", sep="\t")
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])
pelanggan <- data.frame(pelanggan, pelanggan_matrix)
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Profesi <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])
pelanggan$NilaiBelanjaSetahun <-pelanggan$NilaiBelanjaSetahun/1000000
field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")

#Bagian K-Means
set.seed(100)
sse <- sapply (1:10, function(param_k){kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25)$tot.wihtinss})
sse

library(ggplot2)

#Bagian K-Means
set.seed(100)
sse <- sapply(1:10, function(param_k){kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25)$tot.withinss})
jumlah_cluster_max<-10
ssdata=data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster, y=sse)) + geom_line(color="red") + geom_point() + ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") + geom_text(aes(label=format(round(sse,2), nsmall=2)), hjust=-0.2, vjust=-0.5) + scale_x_discrete(limits=c(1:jumlah_cluster_max))

  #"Pemaketan" Model K-Means
#Lengkapi dengan dua vector bernama cluster dan Nama.Segmen
Segmen.Pelanggan <- data.frame(cluster = c(1,2,3,4,5), Nama.Segmen = c("Silver Youth Gals","Diamond Senior Member","Gold Young Professional","Diamond Professional","Silver Mid Professional"))

#Membaca data csv dan dimasukkan ke variable pelanggan
pelanggan <- read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt", sep="\t")
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])
pelanggan <- data.frame(pelanggan, pelanggan_matrix)
pelanggan$NilaiBelanjaSetahun = pelanggan$NilaiBelanjaSetahun/1000000
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Residen <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])

#Bagian K-Means
set.seed(100)
field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")
segmentasi <- kmeans(x=pelanggan[field_yang_digunakan], centers=5, nstart=25)
Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4,5), Nama.Segmen=c("Silver Youth Gals", "Diamond Senior Member", "Gold Young Professional", "Diamond Professional", "Silver Mid Professional"))

#Menggabungkan seluruh aset ke dalam variable Identitas.Cluster
Identitas.Cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis.Kelamin, Tipe.Residen=Tipe.Residen, Segmentasi=segmentasi, Segmen.Pelanggan=Segmen.Pelanggan, field_yang_digunakan=field_yang_digunakan)
saveRDS(Identitas.Cluster,"cluster.rds")

  #Mengoperasionalkan Model K-Means
# Silakan diketikkan code yang dicontohkan dan kemudian hapuslah komentar ini
databaru<- data.frame(Customer_ID="CUST-100", Nama.Pelanggan="Rudi Wilamar", Umur=20, Jenis.Kelamin="Wanita", Profesi="Pelajar", Tipe.Residen="Cluster", NilaiBelanjaSetahun=3.5)
Identitas.Cluster<-readRDS(file="cluster.rds")

#Masukkan perintah untuk penggabungan data
databaru<-merge(databaru, Identitas.Cluster$Profesi)
databaru<- merge(databaru, Identitas.Cluster$Jenis.Kelamin)
databaru<-merge(databaru, Identitas.Cluster$Tipe.Residen)
databaru

#menentukan data baru di cluster mana
which.min(sapply( 1:5, function( x ) sum( ( databaru[Identitas.Cluster$field_yang_digunakan] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) ))
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:5, function ( x )sum( (databaru[Identitas.Cluster$field_yang_digunakan] -Identitas.Cluster$Segmentasi$centers[x,])^2))),]
                                                    
                                                    
