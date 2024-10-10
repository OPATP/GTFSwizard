
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
  cat(red$bold("
    ____  _____   _____    _____
   /        |    |/       /  
  |         |    |       |      
  |  --     |    ------   -----  
  |   |     |    |             | 
   ----     |    |       _____/ 
"))
  
  cat(blue$bold("
|       |  | -----     _     -----  ----
|       |  |  \\       / \\    |    | |    \\
|       |  |   \\     /   \\    --- | |    |
 \\ /\\  /   |    \\   /  -  \\  |    | |   /
  -   -    | ----- /       \\ |    | ----
"))
  
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
