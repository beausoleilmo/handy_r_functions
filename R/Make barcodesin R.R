library(baRcodeR)
baRcodeR::make_labels()
# Or something like this: 
# Load the labels (only thing in the text file) 
label_csv <- read.csv( '~/Desktop/brcd.txt', header = FALSE, stringsAsFactors = FALSE)
# Make the PDF (may take some time )
custom_create_PDF(user=FALSE, 
                  Labels = label_csv[,1], 
                  name = '~/Desktop/LabelsOut', 
                  type = 'linear',#'matrix', 
                  ErrCorr = 'L', 
                  Fsz = 10, # font size 
                  Across = TRUE, 
                  ERows = 0, ECols = 0, 
                  trunc = FALSE, numrow = 50, numcol = 5, 
                  page_width = 8.5, page_height = 11, width_margin = 0.25, height_margin = 0.5, 
                  label_width = NA, label_height = NA, x_space = 0, y_space = 0.5)
