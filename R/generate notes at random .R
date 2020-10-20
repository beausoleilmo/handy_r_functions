# Make notes vector 
notes.all = c(paste0(LETTERS[1:7]),
              paste0(LETTERS[1:7],"#"),
              paste0(LETTERS[1:7],"b"))
notes.sel = notes.all[-c(9,12,17,20)]
notes.M.m = c(notes.sel, paste0(notes.sel,"m"))
notes.M.m7 = c(notes.M.m,paste0(notes.M.m,"7"))
notes.M.m9= c(notes.M.m7,paste0(notes.M.m7,"9"))
# Make it reproducible 
# set.seed(1234567890)
# number of seconds 
seconds = 5
# Show one note at the time 
for (i in 1:5) {
  # Empty plot
  plot(0, 0,
       type = "n",
       axes = F, yaxt = "n", xaxt = "n", 
       ylab = "", xlab = "")
  # Select a note 
  note.sample = sample(x = notes.M.m7,
                       size = 1,
                       replace = TRUE)
  # Print the note 
  print(text(0, 0,
             note.sample, 
             cex = 10))
  # Make the system sleep for X amount of seconds 
  Sys.sleep(seconds) 
}
