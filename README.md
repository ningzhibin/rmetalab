# What is rmetalab

an R package for metaproteomics data analysis
 
## Note: This is the R package, not the Metalab Desktop

rmetalab r package has accompanying functions for metaproteomics data analysis and more

There also some very frequently used handy functions, most of which are self-developed. Some are modified from the internet, with source addressed. 

Developped for metaproteomics.


# changelog

* TODO: a lot of more functions to wrap up
        streamplot
        veroniplot for general purpose
        
        
        
* 20231108 V 0.43 bug fix: if there are no rows with all zeros, there will be a bug returning a empty df_intensity mattrix, do a quick test first,if no rows of all 0, do nothing
        
        
* 20231006 V 0.42 bug fix, when the protein table is imported by readr::read_tsv, intensity columns are most likely converted into charcter, which stops the following calculation. An extra step was applied to the data.frame  (apply(data_table,2, as.numeric)) for this conversion

* 20220528 V 0.41 bug fix in plot_betadiversity dist_matrix_t does not exisits
      

* 20220511 V 0.39, update the document for scatterplot_ggrepel
                  add a new module called ADVANCDED_SCATTER

* 20220511 V 0.38 update function: generate_test_data, with no more column random on matrix, and extra ouput of original meta.
* 20220510 V 0.37 add function: ggplot2_prettier scatterplot_ggrepel 
* 20220510 v 0.36 bug fix for proteingroups with only one raw files, with drop = FLASE
                  bug fix, for plot in box module, with interactive and plotly preload
* 20220419 v 0.34 fixed a bug with in tidy_taxon_table, with a tryCatch
* 20220310 V 0.25 more functions to support the new metareport package 
* 20220228 v 0.17: add functions for meta taxon export
* 20200216 adapt a quickheatmap function and a quickpheatmap function
* 20200215 a bug fix with intensity column pickup 
* 20220214 update the function tidy_peptide_table to make it support the import option with check.names = TRUE. Make it a bit neat as well.

