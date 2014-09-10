Drift Simulation in R for two epistatic traits at four loci
==========================================================

R script showing the distribution of outcomes under genetic drift and occasional recombination in _Avena bartbata_. This simulation is for two traits at four loci. Intention to show distribution of outcomes under neutrality and then compare to actual observations, and infer selection, specifically in the highly-selfing, diploidized tetraploid, _A. barbata_. Two heritable characters of leaf sheath pubescence (dominant character or analogous to dominance) and dark lemma color (dominant or analogous to dominance). Just as a note, Mendelian processes do not lend themselves well to vectorizing for random samples, hence all the "for" loops. Specify popsize, outcrossing rate, and number of generations to run. The two starting genotypes are named 'mesic' and 'xeric' and form the parental generation. 

The simulation may be a bit slow, but speeds up if run in parallel (a variety of options for this). 


