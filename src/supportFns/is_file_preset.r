is_file_preset <- function(f) {
  file.exists(f) | file.exists(gsub("CapeMay", "Cape May", f))
}