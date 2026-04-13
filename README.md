# excessiveSwitching
Excessive switching in OCD and paranoia arise from different deficits in belief updating

Authors:
Charlotte M. Freeland, Praveen Suthaharan, Santiago Castiello de Obeso , Christopher Pittenger Philip R. Corlett

Affiliations: 
Department of Psychiatry and Child Study Center, Yale School of Medicine, New Haven, CT, USA; Wu Tsai Institute, Department of Psychology and Center for
Brain and Mind Health, Yale University, New Haven, CT, USA

*Correspondence: philip.corlett@yale.edu

Study Objective:
We investigated if there is a model-based explanation for reduced perseveration (lose-stay) or excessive switching (win-switch) in participants with self-reported OCD
symptoms. We then compared previously reported HGF parameter estimates (Suthaharan et al., 2021) in participants with clinically significant paranoia and/or OCD to
those in participants with low symptom scores and performed network analyses (Bayesian Gaussian graphical modeling). By applying this combination of computational
methods, we sought to gain insight to latent cognitive mechanisms that underlie, and distinguish, OCD and paranoia. 

The BGGM directory contains R scripts for running BGGMs and .txt summaries for each BGGM, including the posterior mean, posterior standard deviation and probability of each hypothesis for each node to node comparison.

The CODE directory contains R scripts data wrangling, modeling, anaylsis and plotting for each of the main and supplementary figures found in the manuscript. It also custom R functions, including wrangleData and createDiagnosticPlots.

The Clinical_Assessments directory contains the inventories/scales of self-report symptom scores. These are provided for accessibility and reference.

The DATA directory contains the raw .csv data and accompanying data dictionary. External data can be sourced from references in the manuscript or the github repositiory below.

The FIGURES directory contains all .png, .jpeg, .svg and Adobe Illustrator files for all figures in main and supplementary test.

The MODEL_DIAGNOSTICS branch contains for each model comparison, including Q-Q plots and fitted vs model residual.

Questions about code/data? See github repository for Suthaharan et al., 2021: https://github.com/psuthaharan/covid19paranoia
or contact charlotte.freeland@yale.edu or praveen.suthaharan@yale.edu
