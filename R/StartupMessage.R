
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
GTFSwizard.StartupMessage()


GTFSwizard.StartupMessage <- function(){
cat("|==================================\n")
cat("|  ",red$bold("GTFS WIZARD"),"",cyan$italic("version"),cyan$italic(packageVersion("GTFSwizard")),"\n")
cat("|==================================")
}
GTFSwizard.StartupMessage()
