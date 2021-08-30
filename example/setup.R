library(devtools)
library(roxygen2)

## Crate basic R Packages template, work from parent directory
setwd("~/AgroSpace")
create("ASAPI")
## Update documentation, inside -packages- name
setwd("~/AgroSpace/ASAPI")
document()

## Install the package, work from parent directory
setwd("~/AgroSpace")
install("ASAPI")

install.packages("usethis")
library(usethis)
use_mit_license("Tomás Acuña Ruz")
# use_readme_md()

use_news_md()
use_git()
use_test("my-test")
