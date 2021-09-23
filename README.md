
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
#> [1]  3 29
class(isa_s)
#> [1] "isatab"
summary(isa_s)
#> Isatab of type study with 3 samples and 3 nodes.
#> Node Source [ID1] (alpha...)
#>   Characteristics[UUID] [ID2] (All missing)
#>   Characteristics[External links] [ID3] (All unique; x-charite-medgen-blood-book-id:alpha...)
#>   Characteristics[Batch] [ID4] (One value: 1)
#>   Characteristics[Organism] [ID5] (One value: Homo sapiens)
#>     Term Source REF [ID6] (One value: NCBITAXON)
#>     Term Accession Number [ID7] (One value: http://purl.bioontology.org/ontology/NCBITAXON/9606)
#>   Characteristics[Sex] [ID8] (One value: UNKNOWN)
#>   Characteristics[Disease status] [ID9] (One value: UNKNOWN)
#>   Characteristics[OMIM disease] [ID10] (All missing)
#>     Term Source REF [ID11] (All missing)
#>     Term Accession Number [ID12] (All missing)
#>   Characteristics[Orphanet disease] [ID13] (All missing)
#>     Term Source REF [ID14] (All missing)
#>     Term Accession Number [ID15] (All missing)
#>   Characteristics[HPO terms] [ID16] (All missing)
#>     Term Source REF [ID17] (All missing)
#>     Term Accession Number [ID18] (All missing)
#>   Comment[Disease notes] [ID19] (All missing)
#> Node Protocol REF [ID20] (Sample collection...)
#>   Performer [ID21] (All missing)
#> Node Sample [ID22] (alpha-N1...)
#>   Characteristics[External links] [ID23] (All missing)
#>   Characteristics[Cell origin] [ID24] (All missing)
#>     Term Source REF [ID25] (All missing)
#>     Term Accession Number [ID26] (All missing)
#>   Characteristics[Cell type] [ID27] (All missing)
#>     Term Source REF [ID28] (All missing)
#>     Term Accession Number [ID29] (All missing)
print(isa_s)
#> # Color data frame (class colorDF) 29 x 3:
#>  │Source Name [ID1]│Characteristics[UUID] [ID2]
#> 1│alpha            │NA                         
#> 2│beta             │NA                         
#> 3│gamma            │NA                         
#>  │Characteristics[External links] [ID3]│Characteristics[Batch] [ID4]
#> 1│x-charite-medgen-blood-book-id:alpha │1                           
#> 2│x-charite-medgen-blood-book-id:beta  │1                           
#> 3│x-charite-medgen-blood-book-id:gamma │1                           
#>  │Characteristics[Organism] [ID5]│Term Source REF [ID6]
#> 1│Homo sapiens                   │NCBITAXON            
#> 2│Homo sapiens                   │NCBITAXON            
#> 3│Homo sapiens                   │NCBITAXON            
#>  │Term Accession Number [ID7]                        
#> 1│http://purl.bioontology.org/ontology/NCBITAXON/9606
#> 2│http://purl.bioontology.org/ontology/NCBITAXON/9606
#> 3│http://purl.bioontology.org/ontology/NCBITAXON/9606
#>  │Characteristics[Sex] [ID8]│Characteristics[Disease status] [ID9]
#> 1│UNKNOWN                   │UNKNOWN                              
#> 2│UNKNOWN                   │UNKNOWN                              
#> 3│UNKNOWN                   │UNKNOWN                              
#>  │Characteristics[OMIM disease] [ID10]│Term Source REF [ID11]
#> 1│NA                                  │NA                    
#> 2│NA                                  │NA                    
#> 3│NA                                  │NA                    
#>  │Term Accession Number [ID12]│Characteristics[Orphanet disease] [ID13]
#> 1│NA                          │NA                                      
#> 2│NA                          │NA                                      
#> 3│NA                          │NA                                      
#>  │Term Source REF [ID14]│Term Accession Number [ID15]
#> 1│NA                    │NA                          
#> 2│NA                    │NA                          
#> 3│NA                    │NA                          
#>  │Characteristics[HPO terms] [ID16]│Term Source REF [ID17]
#> 1│NA                               │NA                    
#> 2│NA                               │NA                    
#> 3│NA                               │NA                    
#>  │Term Accession Number [ID18]│Comment[Disease notes] [ID19]
#> 1│NA                          │NA                           
#> 2│NA                          │NA                           
#> 3│NA                          │NA                           
#>  │Protocol REF [ID20]│Performer [ID21]│Sample Name [ID22]
#> 1│Sample collection  │NA              │alpha-N1          
#> 2│Sample collection  │NA              │beta-N1           
#> 3│Sample collection  │NA              │gamma-N1          
#>  │Characteristics[External links] [ID23]│Characteristics[Cell origin] [ID24]
#> 1│NA                                    │NA                                 
#> 2│NA                                    │NA                                 
#> 3│NA                                    │NA                                 
#>  │Term Source REF [ID25]│Term Accession Number [ID26]
#> 1│NA                    │NA                          
#> 2│NA                    │NA                          
#> 3│NA                    │NA                          
#>  │Characteristics[Cell type] [ID27]│Term Source REF [ID28]
#> 1│NA                               │NA                    
#> 2│NA                               │NA                    
#> 3│NA                               │NA                    
#>  │Term Accession Number [ID29]
#> 1│NA                          
#> 2│NA                          
#> 3│NA
```

You can directly modify the isatab object almost as simply as you would
do it with a data frame:

``` r
## access a node
isa_s[ "New Node" ] <- c("em", "pstrem", "bzdrem")
#> Adding node New Node after node Sample Name [ID22]
#> New names:
#> * `` -> ...1
#> New ID ID30

## create a property of the new node
isa_s[ "New Node", "Characteristics[Replicate]" ] <- 1:3
#> Modifying / creating properties 'Characteristics[Replicate]'...
#> Creating property Characteristics[Replicate] for node New Node [ID30] after column [ID30]

## remove the node and all its properties
isa_s[ "New Node" ] <- NULL
#> Removing node New Node [ID30]
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
