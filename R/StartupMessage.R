
library('crayon')

GTFSwizard.StartupMessage <- function(){
  cat(red("
  #####   #####  #####   #####  
  #         #    #       #      
  #  ##     #    ####    #####  
  #   #     #    #           #  
  #####     #    #       #####  
"))
  
  cat(red("
#       #  # #####     #     #####  ####
#       #  #  #       # #    #    # #   #
#       #  #   #     #   #   ###### #    #
#  #  # #  #    #   #  #  #  #    # #   #
 #  #  #   # ##### #       # #    # ####
"))

}
cat('V1\n')
GTFSwizard.StartupMessage()


GTFSwizard.StartupMessage <- function(){
  cat(green$bold("
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
cat(cyan$italic("         version"),cyan$italic(packageVersion("GTFSwizard")),"\n\n")
cat(cyan('Type \'citation("GTFSwizard”)\' for citing this R package in publications.'))
  
}
cat('V2\n')
GTFSwizard.StartupMessage()


GTFSwizard.StartupMessage <- function(){
cat("|==================================\n")
cat("|  ",red$bold("GTFS WIZARD"),"",cyan$italic("version"),cyan$italic(packageVersion("GTFSwizard")),"\n")
cat("|==================================")
}
cat('V3\n')
GTFSwizard.StartupMessage()
