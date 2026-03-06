library(readr)
tryCatch({
  read_fwf("non_existent_file.dat", fwf_widths(c(5, 5), c("A", "B")))
}, error = function(e) {
  cat("Error message:", e$message, "\n")
})

# Create a malformed file
writeLines("short", "malformed.dat")
tryCatch({
  # read_fwf might not throw error for short lines, it might just give NAs or warnings
  # Let's try something that definitely fails or gives a verbose message
  read_fwf("malformed.dat", fwf_widths(c(10), c("A")), col_types = cols(A = col_integer()))
}, error = function(e) {
  cat("Error message (malformed):", e$message, "\n")
})
