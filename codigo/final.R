rm(list = ls())

# Packages needed
packages_needed <- c("ggplot2", "Rmisc", "tidyverse", "colorspace", "cowplot",
                     "rcartocolor", "ggforce", "ggdist", "ggridges", "ggimage",
                     "ggbeeswarm", "gghalves", "systemfonts", "grid")

# Install packages not yet installed
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  print(paste(packages_needed[!packages_installed], " not installed!!", sep = " "))
  quit()
} 

packages_loaded <- packages_needed %in% .packages()
if (any(packages_loaded == FALSE)) {
  invisible(
    lapply(
      packages_needed[!packages_loaded], library, character.only = TRUE
    )
  )
}
rm(packages_installed, packages_loaded, packages_needed)

cenarios <- c("fis", "kvm", "cus")
recursos <- c("cpu", "mem", "srd", "swr")
sistemas <- c("lin", "win")

cpu_resultados <- data.frame(matrix(nrow = 0, ncol = 4))
mem_resultados <- data.frame(matrix(nrow = 0, ncol = 4))
srd_resultados <- data.frame(matrix(nrow = 0, ncol = 4))
swr_resultados <- data.frame(matrix(nrow = 0, ncol = 4))

for (recurso in recursos) {
  for (cenario in cenarios) {
    
    auxDF <- read.csv(paste("arquivos/",recurso,"_",cenario,".csv",sep = ""))
    auxDF$test <- rep(cenario, 60)
    
    if (recurso == "cpu") cpu_resultados <- rbind(cpu_resultados, auxDF)
    if (recurso == "mem") mem_resultados <- rbind(mem_resultados, auxDF)
    if (recurso == "srd") srd_resultados <- rbind(srd_resultados, auxDF)
    if (recurso == "swr") swr_resultados <- rbind(swr_resultados, auxDF)
    
  }
}
rm(auxDF, cenario, recurso)

difCusLin <- (cpu_resultados[cpu_resultados$test == "cus" & cpu_resultados$os == "lin",]$time - 
                cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "lin",]$time) / 
             cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "lin",]$time * 100
difCusWin <- (cpu_resultados[cpu_resultados$test == "cus" & cpu_resultados$os == "win",]$time - 
                cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "win",]$time) / 
             cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "win",]$time * 100

difKvmLin <- (cpu_resultados[cpu_resultados$test == "kvm" & cpu_resultados$os == "lin",]$time - 
            cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "lin",]$time) / 
             cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "lin",]$time * 100
difKvmWin <- (cpu_resultados[cpu_resultados$test == "kvm" & cpu_resultados$os == "win",]$time - 
                cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "win",]$time) / 
            cpu_resultados[cpu_resultados$test == "fis" & cpu_resultados$os == "win",]$time * 100

diferenca <- c(rep(0,60), difKvmLin, difKvmWin, difCusLin, difCusWin)
cpu_resultados <- cbind(cpu_resultados, diferenca)
rm(diferenca, difCusLin, difCusWin, difKvmLin, difKvmWin)
cpu_resultados <- cpu_resultados[-c(1:60),]
rownames(cpu_resultados) <- c(1:120)
 
# difCus <- (mem_resultados[mem_resultados$test == "cus",]$time - 
#              mem_resultados[mem_resultados$test == "fis",]$time) / 
#   mem_resultados[mem_resultados$test == "fis",]$time * 100
# difKvm <- (mem_resultados[mem_resultados$test == "kvm",]$time - 
#              mem_resultados[mem_resultados$test == "fis",]$time) / 
#   mem_resultados[mem_resultados$test == "fis",]$time * 100
# diferenca <- c(rep(0,30), difKvm, difCus)
# mem_resultados <- cbind(mem_resultados, diferenca)
# rm(diferenca, difCus, difKvm)
# mem_resultados <- mem_resultados[-c(1:30),]
# rownames(mem_resultados) <- c(1:60)
# 
# difCus <- (srd_resultados[srd_resultados$test == "cus",]$time - 
#              srd_resultados[srd_resultados$test == "fis",]$time) / 
#   srd_resultados[srd_resultados$test == "fis",]$time * 100
# difKvm <- (srd_resultados[srd_resultados$test == "kvm",]$time - 
#              srd_resultados[srd_resultados$test == "fis",]$time) / 
#   srd_resultados[srd_resultados$test == "fis",]$time * 100
# diferenca <- c(rep(0,30), difKvm, difCus)
# srd_resultados <- cbind(srd_resultados, diferenca)
# rm(diferenca, difCus, difKvm)
# srd_resultados <- srd_resultados[-c(1:30),]
# rownames(srd_resultados) <- c(1:60)
# 
# difCus <- (swr_resultados[swr_resultados$test == "cus",]$time - 
#              swr_resultados[swr_resultados$test == "fis",]$time) / 
#   swr_resultados[swr_resultados$test == "fis",]$time * 100
# difKvm <- (swr_resultados[swr_resultados$test == "kvm",]$time - 
#              swr_resultados[swr_resultados$test == "fis",]$time) / 
#   swr_resultados[swr_resultados$test == "fis",]$time * 100
# diferenca <- c(rep(0,30), difKvm, difCus)
# swr_resultados <- cbind(swr_resultados, diferenca)
# rm(diferenca, difCus, difKvm)
# swr_resultados <- swr_resultados[-c(1:30),]
# rownames(swr_resultados) <- c(1:60)
 
gCPU <- ggplot(cpu_resultados, aes(x=id, y=diferenca, group=test, fill=test)) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(0), stat="identity", width = 1, alpha = 0.6,
           color = "black") +
  labs(title = "", x = "Rodadas", y = "Diferença percentual (%)") +
  scale_fill_manual(
    name = "Ambiente:",
    labels = c("Customizado", "Não customizado"),
    values = c("green", "red")
    ) +
  scale_x_discrete(limits=factor(c(1:30))) +
  scale_y_discrete(limits=factor(c(0:4))) +
  facet_wrap(~ os, scales = "free") +
  theme(
    legend.position = "top",
    legend.text = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
    )

# gMEM <- ggplot(mem_resultados, aes(x=id, y=diferenca, group=test, fill=test)) +
#   theme_bw() +
#   geom_hline(yintercept = 0) +
#   geom_bar(position=position_dodge(0), stat="identity", width = 1, alpha = 0.6,
#            color = "black") +
#   labs(title = "", x = "Rodadas", y = "Diferença percentual (%)") +
#   scale_fill_manual(
#     name = "Ambiente:", 
#     labels = c("Customizado", "Não customizado"),
#     values = c("green", "red")
#   ) +
#   scale_x_discrete(limits=factor(c(1:30))) +
#   # scale_y_discrete(limits=factor(seq(0, 25, by=5))) +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(face = "bold", size = 14),
#     legend.title = element_text(face = "bold", size = 14),
#     axis.text = element_text(face = "bold", size = 14),
#     axis.title = element_text(face = "bold", size = 14),
#     strip.text = element_text(face = "bold", size = 14)
#     )
#   
# gSRD <- ggplot(srd_resultados, aes(x=id, y=diferenca, group=test, fill=test)) +
#   theme_bw() +
#   geom_hline(yintercept = 0) +
#   geom_bar(position=position_dodge(0), stat="identity", width = 1, alpha = 0.6,
#            color = "black") +
#   labs(title = "", x = "Rodadas", y = "Diferença percentual (%)") +
#   scale_fill_manual(
#     name = "Ambiente:", 
#     labels = c("Customizado", "Não customizado"),
#     values = c("green", "red")
#   ) +
#   scale_x_discrete(limits=factor(c(1:30))) +
#   theme(
#     legend.position = "botton",
#     legend.text = element_text(face = "bold", size = 14),
#     legend.title = element_text(face = "bold", size = 14),
#     axis.text = element_text(face = "bold", size = 14),
#     axis.title = element_text(face = "bold", size = 14),
#     strip.text = element_text(face = "bold", size = 14)
#     )
# 
# gSWR <- ggplot(swr_resultados, aes(x=id, y=diferenca, group=test, fill=test)) +
#   theme_bw() +
#   geom_hline(yintercept = 0) +
#   geom_bar(position=position_dodge(0), stat="identity", width = 1, alpha = 0.6,
#            color = "black") +
#   labs(title = "", x = "Rodadas", y = "Diferença percentual (%)") +
#   scale_fill_manual(
#     name = "Ambiente:", 
#     labels = c("Customizado", "Não customizado"),
#     values = c("green", "red")
#   ) +
#   scale_x_discrete(limits=factor(c(1:30))) +
#   theme(
#     legend.position = "botton",
#     legend.text = element_text(face = "bold", size = 14),
#     legend.title = element_text(face = "bold", size = 14),
#     axis.text = element_text(face = "bold", size = 14),
#     axis.title = element_text(face = "bold", size = 14),
#     strip.text = element_text(face = "bold", size = 14)
#     )

# cpu_ci <- data.frame(matrix(nrow = 0, ncol = 5))
# mem_ci <- data.frame(matrix(nrow = 0, ncol = 5))
# srd_ci <- data.frame(matrix(nrow = 0, ncol = 5))
# swr_ci <- data.frame(matrix(nrow = 0, ncol = 5))
# 
# for (recurso in recursos) {
#   for (cenario in cenarios) {
#     for (sistema in sistemas) {
#     
#       if (recurso == "cpu") {
#         auxCI <- Rmisc::CI(cpu_resultados[cpu_resultados$test == cenario & 
#                                  cpu_resultados$os == sistema, ]$time)
#         cpu_ci <- rbind(cpu_ci, c(cenario, sistema, auxCI))
#       }
#       if (recurso == "mem") {
#         auxCI <- Rmisc::CI(mem_resultados[mem_resultados$test == cenario & 
#                                  mem_resultados$os == sistema, ]$time)
#         mem_ci <- rbind(mem_ci, c(cenario, sistema, auxCI))
#       }
#       if (recurso == "srd") {
#         auxCI <- Rmisc::CI(srd_resultados[srd_resultados$test == cenario & 
#                                  srd_resultados$os == sistema, ]$time)
#         srd_ci <- rbind(srd_ci, c(cenario, sistema, auxCI))
#       }
#       if (recurso == "swr") {
#         auxCI <- Rmisc::CI(swr_resultados[swr_resultados$test == cenario & 
#                                  swr_resultados$os == sistema, ]$time)
#         swr_ci <- rbind(swr_ci, c(cenario, sistema, auxCI))
#       }
#       
#     }
#   }
# }
# rm(auxCI, cenario, sistema, recurso)
# 
# colnames(cpu_ci) <- c("cenario", "sistema", "superior", "media", "inferior")
# colnames(mem_ci) <- c("cenario", "sistema", "superior", "media", "inferior")
# colnames(srd_ci) <- c("cenario", "sistema", "superior", "media", "inferior")
# colnames(swr_ci) <- c("cenario", "sistema", "superior", "media", "inferior")
# 
# gCPU <- ggplot(cpu_ci[cpu_ci$sistema == "lin",], aes(x=cenario, y=media, group=sistema, fill=sistema)) +
#   theme_bw() +
#   geom_bar(position=position_dodge(0.6), stat="identity", width = 0.5) +
#   geom_errorbar(
#     aes(ymin=inferior, ymax=superior), 
#     width = 0.2,
#     position=position_dodge(0.6)
#     )
# 
# gMEM <- ggplot(mem_ci, aes(x=cenario, y=media, group=sistema, fill=sistema)) +
#   theme_bw() +
#   geom_bar(position=position_dodge(0.6), stat="identity", width = 0.5) +
#   geom_errorbar(
#     aes(ymin=inferior, ymax=superior), 
#     width = 0.2,
#     position=position_dodge(0.6)
#   )
# 
# gSRD <- ggplot(srd_ci, aes(x=cenario, y=media, group=sistema, fill=sistema)) +
#   theme_bw() +
#   geom_bar(position=position_dodge(0.6), stat="identity", width = 0.5) +
#   geom_errorbar(
#     aes(ymin=inferior, ymax=superior), 
#     width = 0.2,
#     position=position_dodge(0.6)
#   )
# 
# gSWR <- ggplot(swr_ci, aes(x=cenario, y=media, group=sistema, fill=sistema)) +
#   theme_bw() +
#   geom_bar(position=position_dodge(0.6), stat="identity", width = 0.5) +
#   geom_errorbar(
#     aes(ymin=inferior, ymax=superior), 
#     width = 0.2,
#     position=position_dodge(0.6)
#   )

# rm(cenarios, recursos, sistemas)