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

cpu_filtrados <- cpu_resultados[cpu_resultados$os == "lin",]
mem_filtrados <- mem_resultados[mem_resultados$os == "lin",]
srd_filtrados <- srd_resultados[srd_resultados$os == "lin",]
swr_filtrados <- swr_resultados[swr_resultados$os == "lin",]

difCus <- (cpu_filtrados[cpu_filtrados$test == "cus",]$time - 
            cpu_filtrados[cpu_filtrados$test == "fis",]$time) / 
            cpu_filtrados[cpu_filtrados$test == "fis",]$time * 100
difKvm <- (cpu_filtrados[cpu_filtrados$test == "kvm",]$time - 
            cpu_filtrados[cpu_filtrados$test == "fis",]$time) / 
            cpu_filtrados[cpu_filtrados$test == "fis",]$time * 100
diferenca <- c(rep(0,30), difKvm, difCus)
cpu_filtrados <- cbind(cpu_filtrados, diferenca)
rm(diferenca, difCus, difKvm)
cpu_filtrados <- cpu_filtrados[-c(1:30),]
rownames(cpu_filtrados) <- c(1:60)

difCus <- (mem_filtrados[mem_filtrados$test == "cus",]$time - 
             mem_filtrados[mem_filtrados$test == "fis",]$time) / 
  mem_filtrados[mem_filtrados$test == "fis",]$time * 100
difKvm <- (mem_filtrados[mem_filtrados$test == "kvm",]$time - 
             mem_filtrados[mem_filtrados$test == "fis",]$time) / 
  mem_filtrados[mem_filtrados$test == "fis",]$time * 100
diferenca <- c(rep(0,30), difKvm, difCus)
mem_filtrados <- cbind(mem_filtrados, diferenca)
rm(diferenca, difCus, difKvm)
mem_filtrados <- mem_filtrados[-c(1:30),]
rownames(mem_filtrados) <- c(1:60)

difCus <- (srd_filtrados[srd_filtrados$test == "cus",]$time - 
             srd_filtrados[srd_filtrados$test == "fis",]$time) / 
  srd_filtrados[srd_filtrados$test == "fis",]$time * 100
difKvm <- (srd_filtrados[srd_filtrados$test == "kvm",]$time - 
             srd_filtrados[srd_filtrados$test == "fis",]$time) / 
  srd_filtrados[srd_filtrados$test == "fis",]$time * 100
diferenca <- c(rep(0,30), difKvm, difCus)
srd_filtrados <- cbind(srd_filtrados, diferenca)
rm(diferenca, difCus, difKvm)
srd_filtrados <- srd_filtrados[-c(1:30),]
rownames(srd_filtrados) <- c(1:60)

difCus <- (swr_filtrados[swr_filtrados$test == "cus",]$time - 
             swr_filtrados[swr_filtrados$test == "fis",]$time) / 
  swr_filtrados[swr_filtrados$test == "fis",]$time * 100
difKvm <- (swr_filtrados[swr_filtrados$test == "kvm",]$time - 
             swr_filtrados[swr_filtrados$test == "fis",]$time) / 
  swr_filtrados[swr_filtrados$test == "fis",]$time * 100
diferenca <- c(rep(0,30), difKvm, difCus)
swr_filtrados <- cbind(swr_filtrados, diferenca)
rm(diferenca, difCus, difKvm)
swr_filtrados <- swr_filtrados[-c(1:30),]
rownames(swr_filtrados) <- c(1:60)

gCPU <- ggplot(cpu_filtrados, aes(x=id, y=diferenca, group=test, fill=test)) +
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
  theme(
    legend.position = "top",
    legend.text = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
    )

gMEM <- ggplot(mem_filtrados, aes(x=id, y=diferenca, group=test, fill=test)) +
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
  # scale_y_discrete(limits=factor(seq(0, 25, by=5))) +
  theme(
    legend.position = "top",
    legend.text = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
    )
  
gSRD <- ggplot(srd_filtrados, aes(x=id, y=diferenca, group=test, fill=test)) +
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
  theme(
    legend.position = "botton",
    legend.text = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
    )

gSWR <- ggplot(swr_filtrados, aes(x=id, y=diferenca, group=test, fill=test)) +
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
  theme(
    legend.position = "botton",
    legend.text = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
    )

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

rm(cenarios, recursos, sistemas)