save_graph <- function(filename, path, device, ...){
  filename <- paste0(filename, device)
  ggsave(filename, path,wight = 8, height = 16)
}
