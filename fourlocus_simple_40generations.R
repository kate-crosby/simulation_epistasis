rm(list=ls())
popsize <- 1000
totalGen <- 40
Mesic <- c(1,1,1,1,0,0,0,0)
Xeric <- c(0,0,0,0,1,1,1,1)
outcrossingrate <- 0.02
Mesic <- matrix(data=Mesic, nrow=popsize/2, ncol=8, byrow=T)
Xeric <- matrix(data=Xeric, nrow=popsize/2, ncol=8, byrow=T)

parents <- rbind(Mesic, Xeric)

# Outputs defined below
Output = matrix(0,totalGen,7)


		#Start the generation loop
		for(igen in 1:totalGen) 
		{ 
				moms <- parents[sample(nrow(parents), popsize, replace=T),]
				num.outcrossers <- rbinom(1,popsize,outcrossingrate)
				selfers <- popsize - num.outcrossers
				
				Output[igen,1] <- igen
				Output[igen,2] <- popsize
				Output[igen,3] <- num.outcrossers
				Output[igen,4] <- selfers

				sex <- sample(1:popsize, num.outcrossers)
				selfing <- setdiff(1:popsize, sex)		
		
				# Identify those rows
				sex.id <- moms[sex,]
				self.id <- moms[selfing,]
				
				# Start with outcrossing MOTHERS (i.e. NOT POLLEN), i.e. the maternal allele at each locus
				locus1.hair.maternal <- sex.id[,1:2]
				locus2.hair.maternal <- sex.id[,3:4]
				locus1.color.maternal <- sex.id[,5:6]
				locus2.color.maternal <- sex.id[,7:8]

						mom.sex.locus1.hair.allele1 <- NULL
						for(i in 1:nrow(locus1.hair.maternal))
							{  
							mom.sex.locus1.hair.allele1[i] = sample(locus1.hair.maternal[i,],1)
							}

						mom.sex.locus2.hair.allele2 <- NULL
						for(i in 1:nrow(locus2.hair.maternal))
							{
							mom.sex.locus2.hair.allele2[i] = sample(locus2.hair.maternal[i,],1)
							}

						mom.sex.locus1.color.allele1 <- NULL
						for(i in 1:nrow(locus1.color.maternal))
							{  
							mom.sex.locus1.color.allele1[i] = sample(locus1.color.maternal[i,],1)
							}

						mom.sex.locus2.color.allele2 <- NULL
						for(i in 1:nrow(locus2.color.maternal))
							{
							mom.sex.locus2.color.allele2[i] = sample(locus2.color.maternal[i,],1)
							}

	# Pollen  
			dads <- sample(1:popsize,num.outcrossers)
			dads.id <- parents[dads,]

				locus1.hair.pollen <- dads.id[,1:2]
				locus2.hair.pollen <- dads.id[,3:4]
				locus1.color.pollen <- dads.id[,5:6]
				locus2.color.pollen <- dads.id[,7:8]

					dad.sex.hair.locus1allele1 <- NULL
					for(i in 1:nrow(locus1.hair.pollen))
						{
  							dad.sex.hair.locus1allele1[i] = sample(locus1.hair.pollen[i,],1)
						}
						
					dad.sex.hair.locus2allele2 <- NULL
					for(i in 1:nrow(locus2.hair.pollen))
						{
							dad.sex.hair.locus2allele2[i] = sample(locus2.hair.pollen[i,],1)
						}
					
					dad.sex.color.locus1allele1 <- NULL
					for(i in 1:nrow(locus1.color.pollen))
							{
	  							dad.sex.color.locus1allele1[i] = sample(locus1.color.pollen[i,],1)
							}

					dad.sex.color.locus2allele2 <- NULL
					for(i in 1:nrow(locus2.color.pollen))
							{
								dad.sex.color.locus2allele2[i] = sample(locus2.color.pollen[i,],1)
							}
# Then cbind the alleles for locus 1 and locus 2 together making a new dataframe

				new.outcrossed.progeny <- data.frame(cbind(mom.sex.locus1.hair.allele1,
					dad.sex.hair.locus1allele1, mom.sex.locus2.hair.allele2, 
					dad.sex.hair.locus2allele2, mom.sex.locus1.color.allele1,
					dad.sex.color.locus1allele1, mom.sex.locus2.color.allele2,
					dad.sex.color.locus2allele2))

# For the selfers - use "self.id" array, and define each locus
			locus1.hair.selfer <- self.id[,1:2]
			locus1.hair.selfer <- as.matrix(locus1.hair.selfer)
			locus2.hair.selfer <- self.id[,3:4]
			locus2.hair.selfer <- as.matrix(locus2.hair.selfer)
			locus1.color.selfer <- self.id[,5:6]
			locus1.color.selfer <- as.matrix(locus1.color.selfer)
			locus2.color.selfer <- self.id[,7:8]
			locus2.color.selfer <- as.matrix(locus2.color.selfer)

# Choose alleles

							locus1.hair.allele1 <- NULL
							for(i in 1:nrow(locus1.hair.selfer))
								{
			  					locus1.hair.allele1[i] <- sample(locus1.hair.selfer[i],1,replace =T)
								}

							locus1.hair.allele2 <- NULL
							for(i in 1:nrow(locus1.hair.selfer))
								{
			  					locus1.hair.allele2[i] <- sample(locus1.hair.selfer[i],1,replace =T)
								}

							locus2.hair.allele1 <- NULL
								for(i in 1:nrow(locus2.hair.selfer))
								{
			  					locus2.hair.allele1[i]  <- sample(locus2.hair.selfer[i,],1,replace =T)
								}

							locus2.hair.allele2 <- NULL
							for(i in 1:nrow(locus2.hair.selfer))
								{
			  					locus2.hair.allele2[i]  <- sample(locus2.hair.selfer[i,],1,replace =T)
								}

							locus1.color.allele1 <- NULL
							for(i in 1:nrow(locus1.color.selfer))
								{
				  				locus1.color.allele1[i] <- sample(locus1.color.selfer[i,],1,replace =T)
								}

							locus1.color.allele2 <- NULL
							for(i in 1:nrow(locus1.color.selfer))
								{
				  				locus1.color.allele2[i] <- sample(locus1.color.selfer[i],1,replace =T)
								}

							locus2.color.allele1 <- NULL
								for(i in 1:nrow(locus2.color.selfer))
								{
				  				locus2.color.allele1[i]  <- sample(locus2.color.selfer[i,],1, replace =T)
								}

							locus2.color.allele2 <- NULL
								for(i in 1:nrow(locus2.color.selfer))
								{
				  				locus2.color.allele2[i]  <- sample(locus2.color.selfer[i,],1, replace =T)
								}


					# Make the array of the selfed progeny
					new.selfed.progeny <- data.frame(cbind(locus1.hair.allele1,
			                                       	locus1.hair.allele2, 
													locus2.hair.allele1, 
													locus2.hair.allele2,
													locus1.color.allele1,
			                                       	locus1.color.allele2, 
													locus2.color.allele1,
													locus2.color.allele2))
					
					# Combine arrays, just use column names from selfed progeny
					new.generation <- rbind(new.selfed.progeny, setNames(new.outcrossed.progeny, names(new.selfed.progeny)))
                    new.generation <- as.matrix(new.generation)              
					
					homs <- subset(new.generation, locus1.hair.allele1 == locus1.hair.allele2 & 
						locus2.hair.allele1==locus2.hair.allele2 & 
						locus1.color.allele1==locus1.color.allele2 & 
						locus2.color.allele1==locus2.color.allele2)
					
					dim.homs<-dim(homs)
					
					sum.homs <- dim.homs[1]
					
					hets <- popsize - sum.homs
					
					pub <- rowSums(new.generation[,1:4])>0
					dark <- rowSums(new.generation[,5:8])>0
					darkpubrec <- pub&dark
					darkpubrec <- sum(darkpubrec)
					
					Output[igen,5] <- sum.homs
					Output[igen,6] <- hets
					Output[igen,7] <- darkpubrec
					

			parents = new.generation
		}

Output
