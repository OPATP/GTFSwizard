


GTFSwizard.StartupMessage <- function(){
  cat(crayon::green$bold("
   _____ _______ ______ _____   
  / ____|__   __|  ____/ ____|  
 | |  __   | |  | |__ | (___    
 | | |_ |  | |  |  __| \\___ \\   
 | |__| |  | |  | |    ____) |  
  \\_____|  __|  |_|   |_____/ _ 
          (_)                | |
 __      ___ ______ _ _ __ __| |
 \\ \\ /\\ / | |_  / _` | '__/ _` |
  \\ V  V /| |/ | (_| | | | (_| |
   \\_/\\_/ |_/___\\__,_|_|  \\__,_|
                                
"))
cat(crayon::cyan$italic("         version"),crayon::cyan$italic(utils::packageVersion("GTFSwizard")),"\n\n")
cat(crayon::cyan('Type \'citation("GTFSwizard”)\' for citing this R package in publications.'))
  
}
