# === 1. Import Data ===
library(readxl)
library(dplyr)
library(stringr)

# Ganti 'data_awal.xlsx' dengan nama file kamu
# Pastikan sheet pertama berisi data asli
data_raw <- read_excel("DATA MENTAH AHP FINAL.xlsx", sheet = "AHP Dataset")

# === 2. Ekstraksi angka dalam tanda kurung ===
# Fungsi untuk ambil angka di dalam tanda kurung dan ubah jadi numerik
extract_value <- function(x) {
  # Ambil teks dalam tanda kurung
  num <- str_extract(x, "\\(([^()]*)\\)")
  num <- str_replace_all(num, "[()]", "") # hapus tanda kurung
  # Ubah jadi numerik (termasuk pecahan seperti 1/3)
  num <- sapply(num, function(x) as.numeric(eval(parse(text = x))))
}

# Terapkan fungsi ke seluruh kolom
data_clean <- data_raw %>%
  mutate(across(everything(), extract_value))

# === 3. Hitung Geometric Mean untuk setiap kolom ===
geomean <- function(x) {
  x <- x[!is.na(x)]
  exp(mean(log(as.numeric(x))))
}

geo_values <- sapply(data_clean, geomean)

# === 4. Buat Comparison Matrix AHP ===
# Tentukan nama faktor sesuai urutan perbandingan berpasangan
factors <- c("Komunikasi", "Sumber Daya", "Disposisi", "Struktur Birokrasi")

# Buat matriks kosong
n <- length(factors)
comparison_matrix <- matrix(1, nrow = n, ncol = n, dimnames = list(factors, factors))

# Urutan pasangan sesuai kolom dataset:
# 1. Komunikasi vs Sumber Daya
# 2. Komunikasi vs Disposisi
# 3. Komunikasi vs Struktur Birokrasi
# 4. Sumber Daya vs Disposisi
# 5. Sumber Daya vs Struktur Birokrasi
# 6. Disposisi vs Struktur Birokrasi

comparison_matrix[1,2] <- geo_values[1]
comparison_matrix[1,3] <- geo_values[2]
comparison_matrix[1,4] <- geo_values[3]
comparison_matrix[2,3] <- geo_values[4]
comparison_matrix[2,4] <- geo_values[5]
comparison_matrix[3,4] <- geo_values[6]

# Isi bagian bawah dengan kebalikannya
for (i in 1:n) {
  for (j in 1:n) {
    if (i > j) {
      comparison_matrix[i,j] <- 1 / comparison_matrix[j,i]
    }
  }
}

# === 5. Simpan hasil ke sheet baru ===
library(openxlsx)
wb <- loadWorkbook("DATA MENTAH AHP FINAL.xlsx")
addWorksheet(wb, "AHP Bersih")
writeData(wb, sheet = "AHP Bersih", data_clean)

addWorksheet(wb, "Geomean")
writeData(wb, sheet = "Geomean", as.data.frame(t(geo_values)))

addWorksheet(wb, "Matrix_AHP")
writeData(wb, sheet = "Matrix_AHP", as.data.frame(comparison_matrix))

saveWorkbook(wb, "DATA MENTAH AHP FINAL.xlsx", overwrite = TRUE)

# === 6. (Opsional) Tampilkan hasil di console ===
print("Nilai Geomean tiap kolom:")
print(geo_values)

print("Comparison Matrix AHP:")
print(comparison_matrix)
