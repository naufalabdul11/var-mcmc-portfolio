library(fitdistrplus)
library(logspline)
library(dplyr)
library(ggplot2)
library(MASS)
library(openxlsx)

#Import data
tbla <- read.xlsx("data/Data Historis TBLA.xlsx")
lsip <- read.xlsx("data/Data Historis LSIP.xlsx")
smar <- read.xlsx("data/Data Historis SMAR.xlsx")

#Pre-processing

#1.Cek missing value
colSums(is.na(tbla))
colSums(is.na(lsip))
colSums(is.na(smar))
#Tidak ada missing value

#2.Tipe data 
sapply(tbla, class)
sapply(lsip, class)
sapply(smar, class)
#Semua numeric, seharusnya tanggal memiliki format date

#3. Mengubah kolom tanggal menjadi date
tbla[, 1] <- as.Date(tbla[, 1], origin = "1899-12-30")
lsip[, 1] <- as.Date(lsip[, 1], origin = "1899-12-30")
smar[, 1] <- as.Date(smar[, 1], origin = "1899-12-30")
#Kolom tanggal telah diubah menjadi date

#4.Cek kembali tipe data 
sapply(tbla, class)
sapply(lsip, class)
sapply(smar, class)
#Tipe data kolom tanggal sudah berubah menjadi date

#Analisis Karakteristik Data
#Statistika deskriptif
summary(tbla)
summary(lsip)
summary(smar)

#Time seris harga saham
tbla$Saham <- "TBLA"
lsip$Saham <- "LSIP"
smar$Saham <- "SMAR"
gabung <- bind_rows(tbla, lsip, smar)

ggplot(gabung, aes(x = Tanggal, y = Terakhir, color = Saham)) +
  geom_line(size = 1) +
  labs(
    title = "Time Series Harga Saham TBLA, LSIP, dan SMAR",
    x = "Tanggal",
    y = "Harga Penutupan",
    color = "Saham"
  ) +
  theme_minimal()

#Plot log return
tbla$return <- c(NA, diff(log(tbla$Terakhir)))
lsip$return <- c(NA, diff(log(lsip$Terakhir)))
smar$return <- c(NA, diff(log(smar$Terakhir)))

ggplot(tbla, aes(x = Tanggal, y = return)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Log Return Saham TBLA",
    x = "Tanggal",
    y = "Log Return"
  ) +
  theme_minimal()

ggplot(lsip, aes(x = Tanggal, y = return)) +
  geom_line(color = "green", size = 1) +
  labs(
    title = "Log Return Saham LSIP",
    x = "Tanggal",
    y = "Log Return"
  ) +
  theme_minimal()


ggplot(smar, aes(x = Tanggal, y = return)) +
  geom_line(color = "red", size = 1) +
  labs(
    title = "Log Return Saham SMAR",
    x = "Tanggal",
    y = "Log Return"
  ) +
  theme_minimal()



#Uji kesesuaian distribusi

# Histogram
hist(tbla$return, main = "Histogram Log Return TBLA", xlab = "Log Return", col = "lightblue")
# QQ plot
qqnorm(tbla$return, main = "QQ Plot TBLA")
qqline(tbla$return, col = "red")

# Histogram
hist(lsip$return, main = "Histogram Log Return LSIP", xlab = "Log Return", col = "lightblue")
# QQ plot
qqnorm(lsip$return, main = "QQ Plot LSIP")
qqline(lsip$return, col = "red")

# Histogram
hist(smar$return, main = "Histogram Log Return SMAR", xlab = "Log Return", col = "lightblue")
# QQ plot
qqnorm(smar$return, main = "QQ Plot SMAR")
qqline(smar$return, col = "red")

shapiro.test(na.omit(tbla$return))
shapiro.test(na.omit(lsip$return))
shapiro.test(na.omit(smar$return))
#Semua data tidak berdistribusi normal


#Fitting Distribusi Return Saham

# Buat vektor numerik dari log return
return_tbla_vector <- as.numeric(na.omit(tbla$return))
return_lsip_vector <- as.numeric(na.omit(lsip$return))
return_smar_vector <- as.numeric(na.omit(smar$return))

descdist(data = return_tbla_vector, discrete = FALSE, boot = 1000)

descdist(data = return_lsip_vector, discrete = FALSE, boot = 1000)

descdist(data = return_smar_vector, discrete = FALSE, boot = 1000)
#Tidak ada distribusi yang sesuai karena semua jauh dari distribusi yang diuji

# Histogram log return
df_hist <- data.frame(
  Return = c(return_tbla_vector, return_lsip_vector, return_smar_vector),
  Saham = factor(c(
    rep("TBLA", length(return_tbla_vector)),
    rep("LSIP", length(return_lsip_vector)),
    rep("SMAR", length(return_smar_vector))
  ))
)

ggplot(df_hist, aes(x = Return, fill = Saham, color = Saham)) +
  geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.8, size = 1) +
  facet_wrap(~Saham, scales = "free") +
  labs(
    title = "Histogram dan Kurva Densitas Log Return Saham",
    x = "Log Return",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


# Fungsi tes AIC dan KS untuk distribusi t, gamma, lognormal
tentukan_distribusi_terbaik <- function(data_vector, nama_saham) {
  cat("Analisis Distribusi:", nama_saham, "\n")
  
  hasil <- list()
  
  # --- Distribusi t ---
  fit_t <- fitdistr(data_vector, densfun = "t")
  aic_t <- 2 * length(fit_t$estimate) - 2 * fit_t$loglik
  pval_t <- ks.test((data_vector - fit_t$estimate["m"]) / fit_t$estimate["s"], 
                    "pt", df = fit_t$estimate["df"])$p.value
  
  hasil[["t"]] <- list(aic = aic_t, pval = pval_t)
  
  # --- Distribusi Log-Normal ---
  data_pos <- data_vector[data_vector > 0]
  if (length(data_pos) > 30) {
    fit_lnorm <- fitdistr(data_pos, "lognormal")
    aic_lnorm <- 2 * length(fit_lnorm$estimate) - 2 * fit_lnorm$loglik
    pval_lnorm <- ks.test(data_pos, "plnorm",
                          meanlog = fit_lnorm$estimate["meanlog"],
                          sdlog   = fit_lnorm$estimate["sdlog"])$p.value
    hasil[["lognormal"]] <- list(aic = aic_lnorm, pval = pval_lnorm)
  }
  
  # --- Distribusi Gamma ---
  if (length(data_pos) > 30) {
    fit_gamma <- fitdistr(data_pos, "gamma")
    aic_gamma <- 2 * length(fit_gamma$estimate) - 2 * fit_gamma$loglik
    pval_gamma <- ks.test(data_pos, "pgamma",
                          shape = fit_gamma$estimate["shape"],
                          rate  = fit_gamma$estimate["rate"])$p.value
    hasil[["gamma"]] <- list(aic = aic_gamma, pval = pval_gamma)
  }
 
  # --- Distribusi Weibull ---
  if (length(data_pos) > 30) {
    fit_weibull <- fitdistr(data_pos, "weibull")
    aic_weibull <- 2 * length(fit_weibull$estimate) - 2 * fit_weibull$loglik
    pval_weibull <- ks.test(data_pos, "pweibull",
                            shape = fit_weibull$estimate["shape"],
                            scale = fit_weibull$estimate["scale"])$p.value
    hasil[["weibull"]] <- list(aic = aic_weibull, pval = pval_weibull)
  }
  
  # Cetak hasil
  df_hasil <- data.frame(
    Distribusi = names(hasil),
    AIC = sapply(hasil, function(x) x$aic),
    P_Value_KS = sapply(hasil, function(x) x$pval)
  )
  
  print(df_hasil)
  
  # Distribusi terbaik berdasarkan AIC (minimum)
  best_aic <- df_hasil$Distribusi[which.min(df_hasil$AIC)]
  # Distribusi terbaik berdasarkan p-value (maksimum)
  best_pval <- df_hasil$Distribusi[which.max(df_hasil$P_Value_KS)]
}

# Contoh untuk ketiga saham
tentukan_distribusi_terbaik(return_tbla_vector, "TBLA")
tentukan_distribusi_terbaik(return_lsip_vector, "LSIP")
tentukan_distribusi_terbaik(return_smar_vector, "SMAR")


#Cek parameter distribusi-t
fit_t <- fitdistr(na.omit(tbla$return), densfun = "t")
fit_t
fit_t <- fitdistr(na.omit(lsip$return), densfun = "t")
fit_t
fit_t <- fitdistr(na.omit(smar$return), densfun = "t")
fit_t
#Cocok dengan distribusi t-student karena memiliki Heavy Tailed 
#dan dilihat dari grafik histogram juga mirip dengan distribusi t-student dengan Heavy Tailed 
#(kasih jurnal yang mendukung kalo saham itu cocok distribusi t)

set.seed(42)

# Fungsi log-likelihood
log_target_density_norm <- function(x, mean, sd) {
  dnorm(x, mean = mean, sd = sd, log = TRUE)
}

# Metropolis-Hastings Sampler untuk distribusi normal
mcmc_sampler_norm <- function(n_iter, init, proposal_sd, mean, sd) {
  samples <- numeric(n_iter)
  samples[1] <- init
  
  for (i in 2:n_iter) {
    proposal <- rnorm(1, mean = samples[i - 1], sd = proposal_sd)
    
    log_accept_ratio <- log_target_density_norm(proposal, mean, sd) -
      log_target_density_norm(samples[i - 1], mean, sd)
    
    if (log(runif(1)) < log_accept_ratio) {
      samples[i] <- proposal
    } else {
      samples[i] <- samples[i - 1]
    }
  }
  
  return(samples)
}

# Parameter saham
params <- list(
  TBLA = list(mean = -0.000845819, sd = 0.009794502),
  LSIP = list(mean =  0.000333249, sd = 0.011771013),
  SMAR = list(mean = -0.00009704055, sd = 0.007625915)
)

iterations <- c(10000)
burn_in_ratio <- 0.2  # 20% burn-in

results <- list()

for (n_iter in iterations) {
  burn_in <- floor(n_iter * burn_in_ratio)
  
  TBLA_sim <- mcmc_sampler_norm(n_iter, 0, 0.005, params$TBLA$mean, params$TBLA$sd)[-(1:burn_in)]
  LSIP_sim <- mcmc_sampler_norm(n_iter, 0, 0.005, params$LSIP$mean, params$LSIP$sd)[-(1:burn_in)]
  SMAR_sim <- mcmc_sampler_norm(n_iter, 0, 0.005, params$SMAR$mean, params$SMAR$sd)[-(1:burn_in)]
  
  return_df <- data.frame(TBLA = TBLA_sim, LSIP = LSIP_sim, SMAR = SMAR_sim)
  
  VaR <- apply(return_df, 2, function(r) quantile(r, probs = c(0.01, 0.05, 0.10)) * 1e9)
  VaR <- round(abs(VaR), 2)
  colnames(VaR) <- c("TBLA", "LSIP", "SMAR")
  rownames(VaR) <- c("VaR_1%", "VaR_5%", "VaR_10%")
  
  results[[paste0("iter_", n_iter)]] <- VaR
}

print(results)

# Hitung Bobot Portofolio Minimum Variance dari Data Historis
# Data historis return
R <- data.frame(
  TBLA = tbla$return,
  LSIP = lsip$return,
  SMAR = smar$return
)
R <- na.omit(R)

# Matriks kovarian & bobot MVEP
cov_matrix <- cov(R)
ones <- rep(1, ncol(R))
inv_cov <- solve(cov_matrix)
w_mvep <- (inv_cov %*% ones) / as.numeric(t(ones) %*% inv_cov %*% ones)
colnames(w_mvep) <- "Weight"
rownames(w_mvep) <- colnames(R)

cat("Bobot MVEP:\n")
print(w_mvep)


# Hitung VaR Portofolio 
weights <- as.numeric(w_mvep)

# Hitung return portofolio simulasi
return_portofolio_simulasi <- as.numeric(as.matrix(return_df) %*% weights)
summary(return_portofolio_simulasi)
descdist(data = return_portofolio_simulasi, discrete = FALSE, boot = 10000)

# Fungsi untuk mengevaluasi distribusi terbaik
tentukan_distribusi_terbaik <- function(data_vector, nama_data = "Data") {
  cat("Analisis Distribusi:", nama_data, "\n")
  
  hasil <- data.frame(
    Distribusi = character(),
    AIC = numeric(),
    P_Value_KS = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Distribusi t-Student
  fit_t <- tryCatch({
    fitdistr(data_vector, densfun = "t")
  }, error = function(e) NULL)
  
  if (!is.null(fit_t)) {
    df <- fit_t$estimate["df"]
    m <- fit_t$estimate["m"]
    s <- fit_t$estimate["s"]
    loglik <- sum(log(dt((data_vector - m)/s, df)/s))
    p_ks <- ks.test((data_vector - m)/s, "pt", df = df)$p.value
    hasil <- rbind(hasil, data.frame(Distribusi = "t", AIC = -2 * loglik + 2 * 3, P_Value_KS = p_ks))
  }
  
  # Distribusi log-normal
  fit_ln <- tryCatch({
    fitdistr(data_vector[data_vector > 0], "lognormal")
  }, error = function(e) NULL)
  
  if (!is.null(fit_ln)) {
    meanlog <- fit_ln$estimate["meanlog"]
    sdlog <- fit_ln$estimate["sdlog"]
    loglik_ln <- sum(dlnorm(data_vector[data_vector > 0], meanlog, sdlog, log = TRUE))
    p_ks_ln <- ks.test(data_vector[data_vector > 0], "plnorm", meanlog, sdlog)$p.value
    hasil <- rbind(hasil, data.frame(Distribusi = "lognormal", AIC = -2 * loglik_ln + 2 * 2, P_Value_KS = p_ks_ln))
  }
  
  # Distribusi gamma
  fit_gamma <- tryCatch({
    fitdistr(data_vector[data_vector > 0], "gamma")
  }, error = function(e) NULL)
  
  if (!is.null(fit_gamma)) {
    shape <- fit_gamma$estimate["shape"]
    rate <- fit_gamma$estimate["rate"]
    loglik_gamma <- sum(dgamma(data_vector[data_vector > 0], shape, rate, log = TRUE))
    p_ks_gamma <- ks.test(data_vector[data_vector > 0], "pgamma", shape, rate)$p.value
    hasil <- rbind(hasil, data.frame(Distribusi = "gamma", AIC = -2 * loglik_gamma + 2 * 2, P_Value_KS = p_ks_gamma))
  }
  
  # Distribusi Weibull
  fit_weibull <- tryCatch({
    fitdistr(data_vector[data_vector > 0], "weibull")
  }, error = function(e) NULL)
  
  if (!is.null(fit_weibull)) {
    shape <- fit_weibull$estimate["shape"]
    scale <- fit_weibull$estimate["scale"]
    loglik_weibull <- sum(dweibull(data_vector[data_vector > 0], shape, scale, log = TRUE))
    p_ks_weibull <- ks.test(data_vector[data_vector > 0], "pweibull", shape = shape, scale = scale)$p.value
    hasil <- rbind(hasil, data.frame(Distribusi = "weibull", AIC = -2 * loglik_weibull + 2 * 2, P_Value_KS = p_ks_weibull))
  }
  
  print(hasil)
}
tentukan_distribusi_terbaik(return_portofolio_simulasi, "Return Portofolio")

# Berdasarkan hasil uji AIC dan KS terbukti bahwa portofolio berdistribusi t, kemudian dapat dilihat juga dari histogram

# Histogram portofolio
df_port <- data.frame(Return = return_portofolio_simulasi)

ggplot(df_port, aes(x = Return)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "skyblue", alpha = 0.5, color = "black") +
  geom_density(color = "red", size = 1.2, alpha = 0.8) +
  labs(
    title = "Histogram dan Kurva Densitas Log Return Portofolio",
    x = "Log Return Portofolio",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


# Simulasi Return Portofolio dengan Metropolis-Hastings (MCMC)

set.seed(42)

# Fungsi densitas target: distribusi t-student
target_density <- function(x, mean, sd, df) {
  dt((x - mean) / sd, df) / sd
}

# Fungsi MCMC Metropolis-Hastings
simulate_mcmc <- function(n_iter = 10000, proposal_sd = 0.002, mean, sd, df) {
  samples <- numeric(n_iter)
  samples[1] <- rnorm(1, mean, sd)
  
  for (i in 2:n_iter) {
    proposal <- rnorm(1, mean = samples[i - 1], sd = proposal_sd)
    alpha <- min(1, target_density(proposal, mean, sd, df) / 
                   target_density(samples[i - 1], mean, sd, df))
    samples[i] <- ifelse(runif(1) < alpha, proposal, samples[i - 1])
  }
  
  return(samples)
}

# Ambil parameter dari return portofolio hasil Monte Carlo sebelumnya
mean_port <- mean(return_portofolio_simulasi)
sd_port <- sd(return_portofolio_simulasi)
df_port <- 2.5  # disesuaikan jika kamu punya hasil fitting distribusi-t

# Jalankan MCMC
mcmc_returns <- simulate_mcmc(n_iter = 10000, proposal_sd = 0.002,
                              mean = mean_port, sd = sd_port, df = df_port)

# Hitung VaR portofolio
VaR_port_mcmc <- quantile(mcmc_returns, probs = c(0.01, 0.05, 0.10)) * 1e9
VaR_port_mcmc <- round(abs(VaR_port_mcmc), 2)
names(VaR_port_mcmc) <- c("VaR_1%", "VaR_5%", "VaR_10%")


cat("VaR Portofolio (MCMC):\n")
print(VaR_port_mcmc)

expected_return_portofolio <- mean(return_portofolio_simulasi)

# Persentase dalam harian
print(round(expected_return_portofolio * 100, 5)) 

# Rupiah dalam harian
expected_return_rupiah <- expected_return_portofolio * 1e9  
print(round(expected_return_rupiah, 2))  

# Done



