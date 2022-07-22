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


