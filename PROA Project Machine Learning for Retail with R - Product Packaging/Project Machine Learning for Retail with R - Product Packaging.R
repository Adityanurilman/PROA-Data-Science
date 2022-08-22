library(arules)

#Reading Dataset
transaksi_tabular <- read.transactions(file="https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
write(transaksi_tabular, file="test_project_retail_1.txt", sep=",")

#Sorting the Top and Bottom 10 Products
all_item <- itemFrequency(transaksi_tabular, type = "absolute")
names(all_item)

top10_item <- sort(all_item, decreasing = TRUE)[1:10]
top10_item <- data.frame("Nama.Produk" = names(top10_item),"Jumlah" = top10_item, row.names = NULL)
write.csv(top10_item, file = "top10_item_retail.txt")
top10_item

bottom10_item <- sort(all_item, decreasing = FALSE)[1:10]
bottom10_item <- data.frame("Nama.Produk" = names(bottom10_item), "Jumlah" = bottom10_item, row.names = NULL)
write.csv(bottom10_item, file = "bottom10_item_retail.txt")
bottom10_item

#Creating The Interesting Product Combinations
kombinasi_retail <- apriori(data = transaksi_tabular,parameter = list(
    support = 10/length(transaksi_tabular), # minimum support
    confidence = 0.50, # minimum confidence  
    minlen = 2, # minimum banyaknya produk dalam satu rule
    maxlen = 3 # maksimum banyaknya produk dalam satu rule
  )
)

kombinasi_retail <- head(sort(kombinasi_retail, decreasing = TRUE,by = 'lift'), n = 10)
write(kombinasi_retail, file = "kombinasi_retail.txt")
kombinasi_retail

#Packaging with Slow Moving Product
kombinasi_retail <- apriori(
  data = transaksi_tabular, 
  parameter = list(
    support = 10/length(transaksi_tabular), # minimum support
    confidence = 0.10, # minimum confidence  
    minlen = 2, # minimum banyaknya produk dalam satu rule
    maxlen = 3 # maksimum banyaknya produk dalam satu rule
  )
)

kombinasi_retail1 <- subset(kombinasi_retail, lift > 1 & rhs %in% "Tas Makeup")
kombinasi_retail1 <- sort(kombinasi_retail1, by='lift', decreasing = T)[1:3]
kombinasi_retail2 <- subset(kombinasi_retail, lift > 1 & rhs %in% "Baju Renang Pria Anak-anak")
kombinasi_retail2 <- sort(kombinasi_retail2, by='lift', decreasing = T)[1:3]
kombinasi_retail <- c(kombinasi_retail1, kombinasi_retail2)
kombinasi_retail

write(kombinasi_retail,file="kombinasi_retail_slow_moving.txt")
