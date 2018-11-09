# LNCDR
Convenience functions for the LNCD

## Install
```R
# install devtools if needed
if (!'devtools' %in% installed.packages()) install.packages('devtools')
# install with devtools
devtools::install_github('LabNeuroCogDevel/LNCDR')
```

## Functions

For help on all functions, in an R console, see `?LNCDR::`<kbd>tab</kbd>

### `pubmed_search`
For meta analysis, get a dataframe of pubmed search results (doi, title, authors, journal, year, abstract)
```R
  btc_papers <- pubmed_search("Tervo-Clemmens[Author]", "tmp_xml/authsearch")

  #  journal  title   year  abstract                                doi   authors
  #  Biologi… Early … 2018  Retrospective neuroimaging studies hav… 10.1… Tervo-C…
  #  NeuroIm… Adoles… 2018  Given prior reports of adverse effects… 10.1… Tervo-C…
  #  Frontie… Neural… 2017  Risk for substance use disorder (SUD) … 10.3… Tervo-C…
  #  Annual … An int… 2015  "Brains systems undergo unique and spe… 10.1… Luna, B…
  #  Journal… Explor… 2013  Comorbid depression and anxiety disord… 10.4… Boyd, R…
```

### `to_nii`
write a nifti file from a voxelwise dataframe
### `zscore` `zscorecols` `zscorewithinfactor`
zscore dataframes
### `col_ungroup`
extract variable grouped columns into rows
```
a.mean b.mean c.mean a.std b.std c.std
     1      2      3    .6    .5   .4

TO

grp  mean  std
a    1     .6
b    2     .5
c    3     .4

```
### `save1D`
given a dataframe with a `block` column and a specified onset column, generate a 1D file for use in afni's `3dDeconvolve`.

### `date_match`
Given two dataframes, both with a column of near matching dates, find the best match between the two.

### `vox_cor`
given a seed region (mask) and target region (mask), give voxelwise correlations from a 4d nifti.

```R
seed <- read_mask("striatum_mask.nii.gz")
target <- read_mask("gm_mask.nii.gz")
target <- target & ! seed
allcors <- vox_cor("subj_ts.nii.gz",seed,target)
```

### `lunaize`
apply Dr. Luna's style to a ggplot. See `?lunaize` for usage.

![lunastyle](img/lunaize-plotcomp.png?raw=true)

### ijk functions
convert ijk indexes between afni and oro MNI(LPI) data matrix.

![afni_ijk](img/afni_ijk.png?raw=true)

```R
x <- oro.nifti::readNIfTI('betas.nii.gz')
dm <- dim(x)
mx <- arrayind(which.max(d),dm)
ijk.oro2afni(mx[1:3], dm )
```

### spectrum functions


![afni](img/afni_shot.png?raw=true)
![spect1](img/spectrum_example.png?raw=true)
![spect2](img/spectrum_thres_example.png?raw=true)

```R
## Simple
colorval <- afni.spectrum(5)
plot_colorspectrum(colorval) 

######################################

## Thresholded and custom axis

png('spectrum_thres_example.png',width=512,height=200);

# Threshold, provide on axis
# limit to a threshold
cv  <- colorval[colorval$invals>=2.68,]
cv$invals[1]<- 2.68 # accurate for data, misleading for scale

# make text 150% of the normal size for the labels
par(cex.lab=1.5)

# plot
plot_colorspectrum(cv,'F',ax=F,side=1) 

# add our own axis
axis(side=1,at=c(2.68,3,4,5),labels=c("2.68","","","5"),las=1)

dev.off()
```

You can also load a custom spectrum by right clicking Olay and saving the spectrum as a jpeg

![getFromAfni](img/afni_shot_savespect.png)
![custumnSpec](img/custom_color.png)

```R
#### custumn color scale
cv <- afni.spectrum(0:5,img='custom_spec.jpg')
plot_colorspectrum(cv,'',side=1)
```

## Building the package and documentation
### Build and test

```
make # see Makefile
```

## Notes

Hilary Parker's package [writeup](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) was used as a template.
```
Rscript -e "devtools::document(); setwd('..'); devtools::install('LNCDR')"
```

Tests follow Hadley Wickham's testthat [description](http://r-pkgs.had.co.nz/tests.html).
