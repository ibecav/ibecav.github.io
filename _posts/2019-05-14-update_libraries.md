---
layout: post
title: Upgrading to R 3.6.0 on a Mac – May 14, 2019
tags: R dplyr functions
---

Every time there is a new major update from [The R
Foundation](https://www.r-project.org/) (like the recent [3.6.0
release](https://cran.r-project.org/) in April). I’m always happy to see
the continuing progress and the combination of new features and bug
fixes, but I also dread the upgrade because it means I have to address
the issue of what to do about the burgeoning number of packages
(libraries) I have installed.

Up until now I confess I simply have sort of “winged it”, done the
upgrade and either manually thought about what packages I **“really”**
needed or just grabbed a few essentials and then let my needs dictate
whatever else I reloaded. This time I decided to get serious about the
process and pay attention to not only what I was doing but documenting
it and keeping a record via some amount of coding (and this post).

I’m aware that there are [full-fledged package
managers](https://blog.learningtree.com/manage-packages-in-r-how/) like
`packrat` and `checkpoint` and even a package designed to manage the
upgrade for you on windows, but I’m a Mac user and wanted to do things
my own way and I don’t need that level of sophistication.

So I set out to do the following:

1.  Capture a list of everything I had installed under `R 3.5.3` and,
    very importantly, as much as I could about where I got the package
    e.g.  `CRAN` or `GitHub` or ???
2.  Keep a copy for my own edification and potential future use.
3.  Do a clean `R 3.6.0` install and not copy any library directories
    manually.
4.  Take a look at the list I produced in \#1 above but mainly to just
    download and install the exact same packages if I can find them.
5.  Make the process mainly scripted and automatic and available again
    for the future.

### Helpful background

As I was searching the web I found a few helpful posts that saved me
time in building my own solution. The primary was [this
post](https://stackoverflow.com/questions/49698348/how-to-find-out-which-package-was-installed-from-github-in-my-r-library)
on `Stack Overflow`. I wanted to extend the function listed there to do
a little more of my work for me. Instead of just being able to generate
a listing of what I had installed from GitHub I wanted to be able to
determine most of the places I get packages from, which are `CRAN`,
`GitHub` and `R-Forge`.

So let’s load `tidyverse` to have access to all it’s various functions
and features and then build a dataframe called `allmypackages` with the
basic information about the packages I currently have installed in R
3.5.3.

**Note - I’m writing this after already upgrading so there will be a few
inconsistencies in the output**

  - This could just as easily be a `tibble` but I chose `as.data.frame`
  - I am deliberately removing base packages from the dataframe by
    `filter`
  - I am eliminating columns I really don’t care about with `select`

<!-- end list -->

``` r
require(tidyverse)
allmypackages <- as.data.frame(installed.packages())
allmypackages <- allmypackages %>%
  filter(Priority != "base" | is.na(Priority)) %>%
  select(-c(Enhances:MD5sum, LinkingTo:Suggests)) %>%
  droplevels()
str(allmypackages)
```

    ## 'data.frame':    533 obs. of  8 variables:
    ##  $ Package         : Factor w/ 533 levels "abind","acepack",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ LibPath         : Factor w/ 1 level "/Library/Frameworks/R.framework/Versions/3.6/Resources/library": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Version         : Factor w/ 361 levels "0.0-8","0.0.1",..: 223 227 57 18 202 352 179 165 49 177 ...
    ##  $ Priority        : Factor w/ 1 level "recommended": NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Depends         : Factor w/ 194 levels "base, stats, R (>= 3.3.0)",..: 25 NA 131 167 NA 145 132 NA NA 97 ...
    ##  $ Imports         : Factor w/ 356 levels "abind, coda, graphics, grDevices, methods, nlme, utils",..: 234 NA 250 75 323 242 1 324 328 336 ...
    ##  $ NeedsCompilation: Factor w/ 2 levels "no","yes": 1 2 1 1 2 2 1 2 1 2 ...
    ##  $ Built           : Factor w/ 1 level "3.6.0": 1 1 1 1 1 1 1 1 1 1 ...

### A function to do the hard work

As I mentioned above the stack overflow post was a good start but I
wanted more information from the function. Rather than TRUE/FALSE to is
it github I would like as much information as possible about where I got
the package. The `package~source` function will be applied to the
`Package` column for each row of our dataframe. For example
`as.character(packageDescription("ggplot2")$Repository)` will get back
“CRAN”, and `as.character(packageDescription("CHAID")$Repository)`
will yield “R-Forge”. For GitHub packages the result is `character(0)`
which has a `length` of zero. So we’ll test with an `if else` clause. If
we get an answer like “CRAN” we’ll just `return` it. If not, we’ll see
if there is a GitHub repo listed with
`as.character(packageDescription(pkg)$GithubRepo)` as well as a GitHub
username `as.character(packageDescription(pkg)$GithubUsername)`. If they
exist we’ll concatenate and return. If not we’ll return “Other”. Besides
being good defensive programming this may catch the package you have
built for yourself as is the case for me.

``` r
package_source <- function(pkg){
  x <- as.character(packageDescription(pkg)$Repository)
  if (length(x)==0) {
    y <- as.character(packageDescription(pkg)$GithubRepo)
    z <- as.character(packageDescription(pkg)$GithubUsername)
    if (length(y)==0) {
      return("Other")
    } else {
      return(str_c("GitHub repo = ", z, "/", y))
    }
  } else {
    return(x)
  }
}
# show the first 60 as an example
head(sapply(allmypackages$Package, package_source), 60)
```

    ##  [1] "CRAN"                            "CRAN"                            "CRAN"                           
    ##  [4] "CRAN"                            "CRAN"                            "CRAN"                           
    ##  [7] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [10] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [13] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [16] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [19] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [22] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [25] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [28] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [31] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [34] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [37] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [40] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [43] "CRAN"                            "CRAN"                            "Other"                          
    ## [46] "R-Forge"                         "CRAN"                            "CRAN"                           
    ## [49] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [52] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [55] "CRAN"                            "CRAN"                            "CRAN"                           
    ## [58] "CRAN"                            "GitHub repo = cjtexas/colourgen" "CRAN"

### What’s in your libraries?

Now that we have the `package_source` function we can add a column to
our data frame and do a little looking.

``` r
allmypackages$whereat <- sapply(allmypackages$Package, package_source)
str(allmypackages)
```

    ## 'data.frame':    533 obs. of  9 variables:
    ##  $ Package         : Factor w/ 533 levels "abind","acepack",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ LibPath         : Factor w/ 1 level "/Library/Frameworks/R.framework/Versions/3.6/Resources/library": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Version         : Factor w/ 361 levels "0.0-8","0.0.1",..: 223 227 57 18 202 352 179 165 49 177 ...
    ##  $ Priority        : Factor w/ 1 level "recommended": NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Depends         : Factor w/ 194 levels "base, stats, R (>= 3.3.0)",..: 25 NA 131 167 NA 145 132 NA NA 97 ...
    ##  $ Imports         : Factor w/ 356 levels "abind, coda, graphics, grDevices, methods, nlme, utils",..: 234 NA 250 75 323 242 1 324 328 336 ...
    ##  $ NeedsCompilation: Factor w/ 2 levels "no","yes": 1 2 1 1 2 2 1 2 1 2 ...
    ##  $ Built           : Factor w/ 1 level "3.6.0": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ whereat         : chr  "CRAN" "CRAN" "CRAN" "CRAN" ...

``` r
table(allmypackages$whereat)
```

    ## 
    ##                                  CRAN       GitHub repo = cjtexas/colourgen 
    ##                                   526                                     1 
    ##     GitHub repo = duncantl/RWordPress         GitHub repo = duncantl/XMLRPC 
    ##                                     1                                     1 
    ##       GitHub repo = leeper/slopegraph GitHub repo = lorenzwalthert/stylermd 
    ##                                     1                                     1 
    ##                                 Other                               R-Forge 
    ##                                     1                                     1

``` r
allmypackages %>% 
  filter(whereat == "Other") %>%
  select(Package, Version)
```

    ##        Package Version
    ## 1 CGPfunctions   0.5.3

And just to be on the safe side we’ll also write a copy out as a csv
file so we have it around in case we ever need to refer back.

``` r
write.csv(allmypackages, "mypackagelistMay2019.csv")
```

### Go ahead and install R 3.6.0

At this point we have what we need, so go ahead and download and install
R 3.6.0. At the end of the installation process you’ll have a pristine
copy with a new library directory. When next you restart R and R Studio
you’ll see a clean new version. Let’s make use of our data frame to
automate most of the process of getting nice clean copies of the
libraries we want.

We’ll start by getting the entire `tidyverse` since we need several
parts and because installing it will trigger the installation of quite a
few dependencies and bootstrap our work.

``` r
# post upgrade with output surpessed
install.packages("tidyverse")
library(tidyverse)
```

Now we have R 3.6.0 and some additional packages. Let’s see what we can
do. First let’s create two dataframes, one with our old list and one
with what we have right now. Then we can use `anti_join` to make a
dataframe that lists the differences `thediff`. We can use `filter` and
`pull` to generate a vector of just the the packages that are on CRAN we
want to install.

**Note – I’m faking the output rather than reinstalling all these
packages on my machine so you will see packages from the `tidyverse` in
the listing **

``` r
oldpackages <- read.csv("mypackagelistMay2019.csv")
allmypackages <- as.data.frame(installed.packages())
allmypackages <- allmypackages %>%
  filter(Priority != "base" | is.na(Priority)) %>%
  select(-c(Enhances:MD5sum, LinkingTo:Suggests))
thediff <- anti_join(oldpackages,allmypackages, by = "Package")
```

    ## Warning: Column `Package` joining factors with different levels, coercing to character vector

``` r
thediff <- droplevels(thediff)
thediff %>%
  filter(whereat == "CRAN") %>%
  pull(Package) %>%
  as.character 
```

    ##  [1] "abind"         "acepack"       "afex"          "alphavantager" "antiword"      "ape"          
    ##  [7] "arm"           "askpass"       "assertthat"    "backports"     "base64enc"     "BayesFactor"  
    ## [13] "bayesplot"     "bayestestR"    "BDgraph"       "beeswarm"      "BH"            "bibtex"       
    ## [19] "bigmemory"     "bigmemory.sri" "bindr"         "bindrcpp"      "bitops"        "blavaan"      
    ## [25] "bookdown"      "Boom"          "BoomSpikeSlab" "boot"          "brew"          "broom"        
    ## [31] "broom.mixed"   "broomExtra"    "BSDA"          "bsts"          "BWStest"       "ca"           
    ## [37] "Cairo"         "callr"         "car"           "carData"       "caret"         "caTools"      
    ## [43] "cdata"         "cellranger"    "checkmate"     "class"         "classInt"      "cli"          
    ## [49] "clipr"         "clisymbols"    "cluster"       "coda"          "codetools"     "coin"         
    ## [55] "colormap"      "colorspace"    "colourpicker"  "combinat"      "commonmark"    "CompQuadForm"

### Just do it\!

Now that you have a nice automated list of everything that is a CRAN
package you can give it a final look and see if there is anything else
you’d like to filter out. Once you are sure the list is right one final
pipe will set the process in motion.

``` r
thediff %>%
  filter(whereat == "CRAN") %>%
  pull(Package) %>%
  as.character %>%
  install.packages
```

Depending on the speed of your network connection and the number of
packages you have that will run for a few minutes.

That takes care of our CRAN packages. What about GitHub?

``` r
thediff %>%
  filter(str_detect(whereat, "GitHub repo")) %>%
  select(Package, Version, NeedsCompilation, whereat)
```

    ##      Package    Version NeedsCompilation                               whereat
    ## 1  colourgen      0.2.0               no       GitHub repo = cjtexas/colourgen
    ## 2 RWordPress      0.2-3               no     GitHub repo = duncantl/RWordPress
    ## 3 slopegraph     0.1.14               no       GitHub repo = leeper/slopegraph
    ## 4   stylermd 0.1.0.9000               no GitHub repo = lorenzwalthert/stylermd
    ## 5     XMLRPC      0.3-1               no         GitHub repo = duncantl/XMLRPC

Here’s another chance to review what you have and whether you still want
need these packages. I could automate the process and once again feed
the right vector to `devtools::install_github()` but instead I choose to
handle these manually as in
`devtools::install_github("leeper/slopegraph")`.

Same with the one package I get from R-Forge…

``` r
allmypackages %>%
  filter(str_detect(whereat, "R-Forge")) %>%
  select(Package, Version, NeedsCompilation, whereat)
install.packages("CHAID", repos="http://R-Forge.R-project.org")
```

At the end of this process you should have a nice clean R install that
has all the packages you choose to maintain as well as a detailed
listing of what those are.

### Done\!

I hope you’ve found this useful. I am always open to comments,
corrections and suggestions.

Chuck (ibecav at gmail dot com)
