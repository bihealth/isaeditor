
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isaeditor

<!-- badges: start -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/bihealth/isaeditor/workflows/R-CMD-check/badge.svg)](https://github.com/bihealth/isaeditor/actions)
<!-- badges: end --> <!-- badges: end -->

isaeditor is a collection of helper functions for modifying and
displaying [ISA-Tab](https://isa-tools.org/) files.

## Installation

You can install the released version of isaeditor from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("isaeditor")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bihealth/isaeditor")
```

## Basic usage

### Reading ISA-Tabs

Use the `read_isa()` function:

``` r
library(isaeditor)
file  <- system.file('extdata', 's_isatab.txt', package='isaeditor')
isa_s <- read_isa(file)
dim(isa_s)
#> [1]  3 15
class(isa_s)
#> [1] "isatab"
summary(isa_s)
#> Isatab of type study with 3 samples and 3 nodes.
#> Node Source [ID1] (alpha...)
#>   Characteristics[Organism] [ID2] (One value: Homo sapiens)
#>     Term Source REF [ID3] (One value: NCBITAXON)
#>     Term Accession Number [ID4] (One value: http://purl.bioontology.org/ontology/NCBITAXON/9606)
#>   Characteristics[Sex] [ID5] (One value: UNKNOWN)
#> Node Protocol REF [ID6] (Sample collection...)
#>   Performer [ID7] (All missing)
#> Node Sample [ID8] (alpha-N1...)
#>   Characteristics[External links] [ID9] (All missing)
#>   Characteristics[Cell origin] [ID10] (All missing)
#>     Term Source REF [ID11] (All missing)
#>     Term Accession Number [ID12] (All missing)
#>   Characteristics[Cell type] [ID13] (All missing)
#>     Term Source REF [ID14] (All missing)
#>     Term Accession Number [ID15] (All missing)
print(isa_s)
#> # Color data frame (class colorDF) 15 x 3:
#>  │Source Name [ID1]│Characteristics[Organism] [ID2]│Term Source REF [ID3]
#> 1│alpha            │Homo sapiens                   │NCBITAXON            
#> 2│beta             │Homo sapiens                   │NCBITAXON            
#> 3│gamma            │Homo sapiens                   │NCBITAXON            
#>  │Term Accession Number [ID4]                        
#> 1│http://purl.bioontology.org/ontology/NCBITAXON/9606
#> 2│http://purl.bioontology.org/ontology/NCBITAXON/9606
#> 3│http://purl.bioontology.org/ontology/NCBITAXON/9606
#>  │Characteristics[Sex] [ID5]│Protocol REF [ID6]│Performer [ID7]
#> 1│UNKNOWN                   │Sample collection │NA             
#> 2│UNKNOWN                   │Sample collection │NA             
#> 3│UNKNOWN                   │Sample collection │NA             
#>  │Sample Name [ID8]│Characteristics[External links] [ID9]
#> 1│alpha-N1         │NA                                   
#> 2│beta-N1          │NA                                   
#> 3│gamma-N1         │NA                                   
#>  │Characteristics[Cell origin] [ID10]│Term Source REF [ID11]
#> 1│NA                                 │NA                    
#> 2│NA                                 │NA                    
#> 3│NA                                 │NA                    
#>  │Term Accession Number [ID12]│Characteristics[Cell type] [ID13]
#> 1│NA                          │NA                               
#> 2│NA                          │NA                               
#> 3│NA                          │NA                               
#>  │Term Source REF [ID14]│Term Accession Number [ID15]
#> 1│NA                    │NA                          
#> 2│NA                    │NA                          
#> 3│NA                    │NA
```

You can directly modify the isatab object almost as simply as you would
do it with a data frame:

``` r
## access a node
isa_s[ "New Node" ] <- c("em", "pstrem", "bzdrem")
#> Adding node New Node after node Sample Name [ID8]
#> New names:
#> * `` -> ...1
#> New ID ID16

## create a property of the new node
isa_s[ "New Node", "Characteristics[Replicate]" ] <- 1:3
#> Modifying / creating properties 'Characteristics[Replicate]'...
#> Creating property Characteristics[Replicate] for node New Node [ID16] after column [ID16]

## remove the node and all its properties
isa_s[ "New Node" ] <- NULL
#> Removing node New Node [ID16]
```

Unfortunately, multiple nodes with the same label may exist according to
the ISA-Tab specifications. Sometimes it is therefore necessary to
indicate which of these nodes we mean. There are several ways to do it
in `isaeditor`, two of them are shown here:

``` r
file <- system.file('extdata', 'a_isatab.txt', package='isaeditor')
isa_a <- read_isa(file)

## use the internal ID to access the node
## you can also use isa_ID_find for that
isa_nodes(isa_a)
#> # A tibble: 6 × 4
#>   node_id node_name    n_properties value_summary                              
#>   <chr>   <chr>               <int> <chr>                                      
#> 1 ID1     Sample Name             1 All unique; alpha-N1...                    
#> 2 ID2     Protocol REF            4 One value: Nucleic acid extraction mRNA_seq
#> 3 ID6     Extract Name            5 All unique; alpha-N1-RNA1...               
#> 4 ID11    Protocol REF           23 One value: Library construction mRNA_seq   
#> 5 ID34    Extract Name            6 All unique; alpha-N1-RNA1-mRNA_seq1...     
#> 6 ID40    Protocol REF            9 One value: Nucleic acid sequencing mRNA_seq
isa_a[['ID34']]
#> [1] "alpha-N1-RNA1-mRNA_seq1" "beta-N1-RNA1-mRNA_seq1" 
#> [3] "gamma-N1-RNA1-mRNA_seq1"

## specify which of the nodes 
isa_a[ "Extract Name", n=2 ]
#> [1] "alpha-N1-RNA1-mRNA_seq1" "beta-N1-RNA1-mRNA_seq1" 
#> [3] "gamma-N1-RNA1-mRNA_seq1"
```
