herring_data <- read.csv("data/herring_data_221116.csv")
sprat_data <- read.csv("data/qis_sprats_baltic_sea/sprat_bits.csv")
ROOT_PATH = getwd()
DATA_PATH = file.path(ROOT_PATH, 'data')
SRC_PATH = file.path(ROOT_PATH, 'R')
OUTPUTS_PATH = file.path(ROOT_PATH, 'outputs')

paper_type <- "a4r"
paper_height <- 8.268
paper_width <- 11.693