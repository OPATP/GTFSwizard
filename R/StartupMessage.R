GTFSwizard.StartupMessage <- function(){
  paste0(
    "
      _____ _______ ______ _____
     / ____|__   __|  ____/ ____|
    | |  __   | |  | |__ | (___
    | | |_ |  | |  |  __| \\___ \\
    | |__| |  | |  | |    ____) |
     \\_____|  |_|  |_|   |_____/ _
    __      ___ ______ _ _ __ __| |
    \\ \\ /\\ / | |_  / _` | '__/ _` |
     \\ V  V /| |/ | (_| | | | (_| |
      \\_/\\_/ |_/___\\__,_|_|  \\__,_|
",
    "######## version ", utils::packageVersion("GTFSwizard"),
    " ##########\n",
    "Type 'citation(\"GTFSwizard\")' for\n",
    "citing this R package in publications."
  )
}
