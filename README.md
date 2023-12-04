# bio.taxonomy

Package to help develop and/or use bio tools in order to ...
 
## Installation


To install, run the following:

```r
  remotes::install_github( "jae0/aegis")  # helper functions
  remotes::install_github( "jae0/bio.taxonomy")
``` 

You probably will want to have an Rprofile set up properly such as:

```r
homedir = path.expand("~")
code_root = file.path( homedir, "bio" )   ### replace with correct path to the parent directory of your git-projects
data_root = file.path( homedir, "bio.data" )   ### replace with correct path to your data

require( aegis )
require( bio.taxonomy )

```
 