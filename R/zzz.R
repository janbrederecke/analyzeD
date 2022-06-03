# Package startup message
.onAttach <- function(libname
                      ,pkgname
){
  
  msg <- paste0(
    "\n",
    "== Thank you for using analyzeD =========================================",
    "\nIf you encounter any bugs feel free to file an issue at:",
    "\nhttps://github.com/janbrederecke/analyzeD",
    "\n",
    "\nYou might want to check my other packages at:",
    "\nhttps://github.com/janbrederecke"
  )
  
  packageStartupMessage(msg)
}