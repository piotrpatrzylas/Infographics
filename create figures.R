#create figures

library(openxlsx)
library(ggplot2)

infographics_workbook <- openxlsx::loadWorkbook("AEC_accompanying_tables_201718_13062018.xlsx")

#data from worksheets

data_clostridium <- read.xlsx(infographics_workbook, sheet = 1, skipEmptyRows = T, skipEmptyCols = T, colNames = F, rowNames = F)
data_mrsa <- read.xlsx(infographics_workbook, sheet = 2, skipEmptyRows = T, skipEmptyCols = T, colNames = F, rowNames = F)
data_mssa <- read.xlsx(infographics_workbook, sheet = 3, skipEmptyRows = T, skipEmptyCols = T, colNames = F, rowNames = F)
data_ecoli <- read.xlsx(infographics_workbook, sheet = 4, skipEmptyRows = T, skipEmptyCols = T, colNames = F, rowNames = F)
data_klebsiella <- read.xlsx(infographics_workbook, sheet = 5, skipEmptyRows = T, skipEmptyCols = T, colNames = F, rowNames = F)
data_pseudomonas <- read.xlsx(infographics_workbook, sheet = 6, skipEmptyRows = T, skipEmptyCols = T, colNames = F, rowNames = F)


#FIGURES
#clostridium
#mrsa
#mssa
#ecoli

df = data_ecoli[c(3:8), c("X1", "X4")]
ggplot(data = df) + geom_line(aes(x = X1, y = X4, group = 1)) + xlab ("Financial year") +
  ylab("Rate, per 1000000 population") + theme_classic() + scale_y_discrete(breaks = seq(1000, 50000, 10))


#klebsiella
#pseudomonas