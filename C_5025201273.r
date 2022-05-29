#1. 
#a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel
#diatas
Responden = c(1,2,3,4,5,6,7,8,9)
x = c(78,75,67,77,70,72,78,74,77)
y = c(100,95,70,90,90,90,89,90,100)

Data = data.frame(Responden,x,y)

print(Data)

deviasi = sd(x-y)

deviasi
#b. carilah nilai t (p-value)

t.test(x,y,alternative = "greater",var.equal = FALSE)

#c. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
#dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
#aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada
#pengaruh yang signifikan secara statistika dalam hal kadar saturasi
#oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
var.test(bfr, afr)t.test(bfr, afr, mu = 0, alternative = "two.sided", var.equal = TRUE)

#2. 
install.packages("BSDA")
library(BSDA)
#a. Apakah Anda setuju dengan klaim tersebut?
#setuju
#b. Jelaskan maksud dari output yang dihasilkan!
zsum.test(mean.x=23500, sigma.x = 3900, n.x = 100,  
          alternative = "greater", mu = 20000,
          conf.level = 0.95)
#c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
# Rata-rata Mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
#3
#A. H0 dan H1
# H0 dan H1
# H0 = 9.50 && H1 = 10.98
#B. Hitung Sampel Statistik
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , 
    s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)
#C. Lakukan Uji Statistik (df =2)
install.packages("mosaic")
library(mosaic)
plotDist(dist = 't', df = 2, col = "blue")
#D. Nilai Kritikal
qchisq(p = 0.05, df = 2, lower.tail = FALSE)
#E. Keputusan
# Teori keputusan adalah teori formal suatu reaksi terhadap beberapa solusi alternatif yang dilakukan secara sadar dengan 
#cara menganalisa kemungkinan- kemungkinan dari alternatif tersebut bersama konsekuensinya
#F. Kesimpulan
# Kesimpulan yang didapatkan yaitu perbedaan rata-rata yang terjadi tidak ada jika 
#dilihat dari uji statistik dan akan ada tetapi tidak signifikan jika dipengaruhi nilai kritikal.


#4
#A. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup
#2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
#lihat apakah ada outlier utama dalam homogenitas varians.

#B. carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
#didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

#C. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
#Grup dan beri nama model tersebut model 1.

#D. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan
#dari H0?

#E. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p
#yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain?
#Jelaskan.

#F. Visualisasikan data dengan ggplot2



#5.
#a. Buatlah plot sederhana untuk visualisasi data

library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read.csv('https://drive.google.com/u/0/uc?id=1aLUOdw_LVJq6VQrQEkuQhZ8FW43FemTJ&export=download')
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

#b. Lakukan uji ANOVA dua arah

GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

#c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
#setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)

data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

#d. Lakukan uji Tukey

tukey <- TukeyHSD(anova)
print(tukey) 

#e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey
