# LNCDR
Convenience functions for the LNCD

![Test_LNCDR](https://github.com/LabNeuroCogDevel/LNCDR/workflows/Test_LNCDR/badge.svg)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6470999.svg)](https://doi.org/10.5281/zenodo.6470999)

## Install
Succinctly, you can install with the command
```R
remotes::install_github('LabNeuroCogDevel/LNCDR')
```

In more detail:
```R
# install the package that handles installing if we haven't already
if (!'remotes' %in% installed.packages()) install.packages('remotes')

# if reinstalling, remove the old package from this workspace
detach("package:LNCDR", unload=TRUE)

# (re)install from most recent online source
remotes::install_github('LabNeuroCogDevel/LNCDR')

# load package
library(LNCDR)
```

## Help
For help on all functions, in an R console, see `?LNCDR::`<kbd>tab</kbd>

## Functions

### Stats

#### [`gam_growthrate`](R/growthrate_gam.R)

For better/more plotting facilities, see [`tidygam`](https://cloud.r-project.org/web/packages/tidygam/vignettes/get-started.html) and [`tidymv`](https://github.com/stefanocoretta/tidymv) (`predict_gam` and [`plot_smooths`](https://cran.r-project.org/web/packages/tidymv/vignettes/plot-smooths.html)), [`gratia`](https://fromthebottomoftheheap.net/2018/10/23/introducing-gratia/), and/or [`ggeffects`](https://strengejacke.github.io/ggeffects/). There's also a nice tutorial by [Bart Larsen](https://github.com/bart-larsen/GAMM-Tutorial/).

![gam_growthrate_plot](img/gam_deriv_btc.png?raw=true)

```R
 m <- gam(f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re"), data=d)
 ci <- gam_growthrate(m, 'Ageatvisit')
 gam_growthrate_plot(d, m, ci, 'Ageatvisit','f1score','id')
```

#### `zscore` `zscorecols` `zscorewithinfactor`
zscore dataframes

#### `lmer_extract`
get values (t, chisq, p) from a single variable in a model

### Plotting

#### [`age_animate`](R/age_animated.R)


[gganimate](https://gganimate.com/) for data frames with an `age` column.




```R
d <- data.frame(age=seq(10,30, length.out=8), x=runif(8), y=runif(8))
p  <- age_animate(d, rep_steps=16) +
      aes(x=x, y=y) +
      labs(title = 'Age: {frameage}')

gif <- p %>% lunaize %>% animate(fps=4, nframes=16, width = 480, height = 480)
anim_save("img/example_animate.gif", gif)
```

![age_animate](img/example_animate.gif?raw=true)


#### `waterfall_plot`

Plot age at each visit for each participant (`id` and `age` column).

```R
data.frame(id=rep(c(1:10),2),
           age=rep(c(11:20),each=2)+runif(20),
           sex=sample(rep(c('M','F'),5))) %>%
  waterfall_plot() +
  aes(color=sex) +
  see::theme_modern()

ggsave("img/waterfall_example.png",dpi=90, units='px', width=600, height=300)
```

![waterfall plot](img/waterfall_example.png?raw=true)

#### `pubmed_search`
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

#### `lunaize`
A better alternative is probably [`cowplot::theme_cowplot()`](https://github.com/wilkelab/cowplot) or [`see::theme_modern()`](https://easystats.github.io/see/#modern)

Apply Dr. Luna's style to a ggplot. See `?lunaize` for usage.

![lunastyle](img/lunaize-plotcomp.png?raw=true)


### Data Wrangling

#### `save1D`
Dataframe to 1D file:

Given a data frame with a `block` column and a specified onset column, generate a 1D file for use in afni's `3dDeconvolve`.

#### `date_match`
Match behavioral visit to scan visit.

Given two dataframes, both with a column of near matching dates, find the best match between the two.

#### `parseROItempcor`
read  [`ROI_TempCorr`](LNCDR/blob/master/tests/testthat/roitempcorr.R) `*.rac1.adj_pearson.txt` outputs into per subject rows.

#### `roicormat_wide`
extract unique pairs from a correlation matrix into a single row.

#### `db_query`
use `.pg_pass` to make quick queries to central database.


#### `col_ungroup`
extract variable grouped columns into rows. DEPRICATED. see `tidyr::pivot_longer` and https://mgimond.github.io/ES218/Week03b.html
```
a.mean b.mean c.mean a.std b.std c.std
     1      2      3    .6    .5   .4

TO

grp  mean  std
a    1     .6
b    2     .5
c    3     .4

```

#### `interactive_label_match`
match labels from one string vector with another

#### `uppsp_scoring`
scores uppsp 59-item


### Imaging

#### `to_nii`
write a nifti file from a voxelwise dataframe

#### `vox_cor`
Given a seed region (mask) and target region (mask), return voxelwise correlations from a 4d nifti.

```R
seed <- read_mask("striatum_mask.nii.gz")
target <- read_mask("gm_mask.nii.gz")
target <- target & ! seed
allcors <- vox_cor("subj_ts.nii.gz",seed,target)
```



#### spectrum functions


![afni](img/afni_shot.png?raw=true)
![spect1](img/spectrum_example.png?raw=true)
```R
afni_save_spectrum(5, thres=2.68, posonly=T, lab="F")
```

![spect2](img/spectrum_thres_example.png?raw=true)

You can also load a custom spectrum by right clicking Olay and saving the spectrum as a jpeg

![getFromAfni](img/afni_shot_savespect.png)
![custumnSpec](img/custom_color.png)

```R
#### custumn color scale
cv <- afni.spectrum(0:5,img='custom_spec.jpg')
plot_colorspectrum(cv,'',side=1)
```

#### ijk functions
Convert ijk indexes between afni and oro MNI(LPI) data matrix.

![afni_ijk](img/afni_ijk.png?raw=true)

```R
x <- oro.nifti::readNIfTI('betas.nii.gz')
dm <- dim(x)
mx <- arrayind(which.max(d),dm)
ijk.oro2afni(mx[1:3], dm )
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
### Adding functions
1. create a new or edit an existing `*.R` file within `R/`.
    - make sure `#' @export` is above a function definition you want exported. See [roxygen primer](https://kbroman.org/pkg_primer/pages/docs.html). You can got to any function using 3 colons:  `LNCDR:::function_without_export`
    - Other functions (esp. [`R/ld8.R`](R/ld8.R)) are a good starting place.
2. run` make`
    - or, in an R console, run: `devtools::document(); devtools::install('./')`
