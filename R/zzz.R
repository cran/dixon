.First.lib <- function(lib, pkg)  {
    library.dynam("dixon", pkg, lib)
    packageStartupMessage("This is an improved (accelerated) version of function dixon, as requested by P.M. Jorgensen. This is ver.  ",
                          utils::packageDescription("dixon", field="Version"),
                          appendLF = TRUE)
}
