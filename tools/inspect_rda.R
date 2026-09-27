# Inspect the committed .rda files directly
for (f in c("segxbar","segp","segc")) {
  e <- new.env()
  load(file.path("data", paste0(f, ".rda")), envir = e)
  obj <- get(ls(e)[1], envir = e)
  cat(f, "-> class", class(obj), " dim", paste(dim(obj), collapse="x"), "\n")
  cat("   str:", paste(strsplit(capture.output(print(str(obj))),"\n"), collapse=" | "), "\n")
}
