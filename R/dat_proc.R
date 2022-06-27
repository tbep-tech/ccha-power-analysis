library(here)

# copy data from ccha-workflow --------------------------------------------

load(url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/vegdat.RData'))
load(url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/tranloc.RData'))

save(tranloc, file = here('data/tranloc.RData'))
save(vegdat, file = here('data/vegdat.RData'))
