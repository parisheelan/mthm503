message("Initializing project setup...")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::activate()


log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", Sys.time(), msg), file = "renv_install.log", append = TRUE)
}

install_if_missing <- function(pkg, type = "source") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    log_msg(paste("Installing", pkg))
    renv::install(pkg, type = type)
    log_msg(paste(pkg, "installed"))
  } else {
    log_msg(paste(pkg, "already installed — skipping"))
  }
}


# Force install stringi on Debian machine (assumes nix)
if (Sys.info()[["sysname"]] == "Linux" && grepl("debian", tolower(system("lsb_release -ds", intern = TRUE)))) {
  message("Detected Debian Linux. Checking on stringi...")
  install_if_missing("stringi")
}

# Source install systemfonts on Debian (assumes nix)  may fail to load with binary if system libs differ

if (Sys.info()[["sysname"]] == "Linux" && grepl("debian", tolower(system("lsb_release -ds", intern = TRUE)))) {
  message("Detected Debian Linux. Checking on systemfonts...")
  install_if_missing("systemfonts")
}

# Restore all other packages
renv::restore()