# Package startup message
.onAttach <- function(libname
                      ,pkgname
){
  
  msg <- paste0(
    "\n",
    "== Thank you for using analyzeD =========================================",
    "\nIf you encounter any bugs feel free to file an issue at:",
    "\n https://github.com/janbrederecke/analyzeD"
  )
  
  packageStartupMessage(msg)
}