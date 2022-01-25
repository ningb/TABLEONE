#' Calculate numbers and statistics for table one
#'
#' @param main A character for the variable names of interests.
#' @param variable.id A numeric/integer vector indicating the variables to compare.
#' @param dataset A data.frame, the annotation data which table one will be generated from.
#' @return A data frame for table one visualization or export.
#' @examples
#' output <- tableone("Cancer", c(1:5), index) # Binary main variable
#' output <- tableone("Smoking", c(1:4, 6), index) # Categorical main variable
#' output <- tableone("RIN", c(2:6), index) # Continuous main variable
#' 
#' @export
#' 
tableone <- function(main, variable.id, dataset) {

	# Check for all inputs
	if(is.null(main) | is.null(variable.id) | is.null(dataset)) {
		stop("Missing one of the three inputs...")
	} 

	# Check input formats
	if(class(main) != "character" | !class(variable.id) %in% c("numeric", "integer") | class(dataset) != "data.frame") {
		stop("Input format wrong...")
	} 

	# Check dimensions and whether indexes are in the dataframe
	if(!main %in% names(dataset) | max(variable.id) > ncol(dataset)) {
		stop("Variables not found in the input dataset...")
	} 

	# Check if the main variable of interests is numeric
	# If so, test association by other groups via t-test/anova/correlation
	if (is.numeric(dataset[,main])){
		out.table <- data.frame(Variable=character(), Statistics=character(), Pval=character(), stringsAsFactors=FALSE)
		for (i in variable.id) {
			if (is.numeric(dataset[,i])) {
				this.dat <- dataset[,c(main, names(dataset)[i])]
				this.res <- cor.test(this.dat[,1], this.dat[,2])
				this.out <- c(names(dataset)[i], as.character(round(this.res$estimate, digits=4)), as.character(round(this.res$p.value, digits=4)))
				out.table[nrow(out.table)+1, ] <-this.out
			} else {
				dataset[,i] <- as.factor(dataset[,i])
				this.dat <- dataset[,c(main, names(dataset)[i])]

				if ( length(levels(this.dat[,names(dataset)[i]])) == 2 ){
					this.res <- t.test(this.dat[,1] ~ this.dat[,2])
					this.out <- c(paste(names(dataset)[i], "; Ref=", levels(this.dat[,2])[1], sep=""), 
								as.character(round(this.res$statistic, digits=4)), as.character(round(this.res$p.value, digits=4)))
					out.table[nrow(out.table)+1, ] <-this.out

				} else {
					this.res <- summary(aov(this.dat[,1] ~ this.dat[,2]))
					this.out <- c(names(dataset)[i], as.character(round(this.res[[1]][1,4], digits=4)), as.character(round(this.res[[1]][1,5], digits=4)))
					out.table[nrow(out.table)+1, ] <-this.out
				}
			}
			print(names(dataset[i]))
		}

	# If the main variable of interests is categorical
	} else {
		if(!is.factor(dataset[,main])) {dataset[,main] <- factor(dataset[,main])}
		
		# whether there are two levels in main variable of interests
		if( length(levels(dataset[,main])) == 2) {
			id <- which(names(dataset) == main)
			out.table <- matrix(nrow=0, ncol=(3+length(levels(dataset[,main]))))
			name_and_N <- paste(levels(dataset[,main]), " (N=", table(dataset[,main]), ")", sep="")
			colnames(out.table) <- c("Variable", name_and_N, "Statistics", "Pval")
			out.table <- as.data.frame(out.table)

			for (i in variable.id) {
				if (is.numeric(dataset[,i])) {
					this.dat <- dataset[,c(main, names(dataset)[i])]
					this.res <- t.test(this.dat[,2] ~ this.dat[,1])
					stat.table <- cbind(aggregate(this.dat[,2], list(this.dat[,1]), function(x) mean(x, na.rm=T)), aggregate(this.dat[,2], list(this.dat[,1]), function(x) sd(x, na.rm=T)))
					this.out <- c(names(dataset)[i], 
								apply(stat.table, 1, function(x) paste(round(as.numeric(x[2]),digit=2), " (", round(as.numeric(x[4]),digit=2), ")", sep="")),
								as.character(round(this.res$statistic, digits=4)), as.character(round(this.res$p.value, digits=4)))
					out.table[nrow(out.table)+1, ] <- this.out
				} else {
					dataset[,i] <- as.factor(dataset[,i])
					this.dat <- dataset[,c(main, names(dataset)[i])]
					if ( length(levels(this.dat[,names(dataset)[i]])) == 2 ){
						this.res <- fisher.test(this.dat[,1], this.dat[,2])
						stat.table <- cbind(table(this.dat[,1], this.dat[,2]), prop.table(table(this.dat[,1], this.dat[,2]), 1))
						this.out <- c(paste(names(dataset)[i], "; Ref=", levels(this.dat[,2])[1], sep=""), 
									apply(stat.table, 1, function(x) paste(round(as.numeric(x[1]),digit=2), " (", round(as.numeric(x[3]*100),digit=2), "%)", sep="")),
									as.character(round(this.res$estimate, digits=4)), as.character(round(this.res$p.value, digits=4)))
						out.table[nrow(out.table)+1, ] <-this.out
					} else {
						this.res <- chisq.test(this.dat[,1], this.dat[,2])
						a <- table(this.dat[,1], this.dat[,2])
						b <- prop.table(table(this.dat[,1], this.dat[,2]), 1)
						stat.table <- t(matrix(paste(a, " (", round(b*100, digits=2), "%)", sep=""), nrow=length(levels(this.dat[,1])), ncol=length(levels(this.dat[,2]))))
						stat.table <- cbind(levels(this.dat[,2]), stat.table)
						this.res <- c(paste(names(dataset)[i], "; Ref=", levels(this.dat[,2])[1], sep=""), 
									rep(NA, length(levels(this.dat[,1]))),
									as.character(round(this.res$statistic, digits=4)), as.character(round(this.res$p.value, digits=4)))
						out.table[nrow(out.table)+1, ] <- this.res
						out.table[(nrow(out.table)+1):(nrow(out.table)+(length(levels(this.dat[,2])))), 1:(1+length(levels(this.dat[,1])))] <- stat.table
					}
				}
				print(names(dataset[i]))
			}
		
		# whether there are more than two levels in main variable of interests
		} else {
			id <- which(names(dataset) == main)
			out.table <- matrix(nrow=0, ncol=(3+length(levels(dataset[,main]))))
			name_and_N <- paste(levels(dataset[,main]), " (N=", table(dataset[,main]), ")", sep="")
			colnames(out.table) <- c("Variable", name_and_N, "Statistics", "Pval")
			out.table <- as.data.frame(out.table)

			for (i in variable.id) {
				if (is.numeric(dataset[,i])) {
					this.dat <- dataset[,c(main, names(dataset)[i])]
					this.res <- summary(aov(this.dat[,2] ~ this.dat[,1]))
					stat.table <- cbind(aggregate(this.dat[,2], list(this.dat[,1]), function(x) mean(x, na.rm=T)), aggregate(this.dat[,2], list(this.dat[,1]), function(x) sd(x, na.rm=T)))
					this.out <- c(names(dataset)[i], 
								apply(stat.table, 1, function(x) paste(round(as.numeric(x[2]),digit=2), " (", round(as.numeric(x[4]),digit=2), ")", sep="")),
								as.character(round(this.res[[1]][1,4], digits=4)), as.character(round(this.res[[1]][1,5], digits=4)))
					out.table[nrow(out.table)+1, ] <- this.out
				} else {
					dataset[,i] <- as.factor(dataset[,i])
					this.dat <- dataset[,c(main, names(dataset)[i])]
					this.res <- chisq.test(this.dat[,1], this.dat[,2])
					a <- table(this.dat[,1], this.dat[,2])
					b <- prop.table(table(this.dat[,1], this.dat[,2]), 1)
					stat.table <- t(matrix(paste(a, " (", round(b*100, digits=2), "%)", sep=""), nrow=length(levels(this.dat[,1])), ncol=length(levels(this.dat[,2]))))
					stat.table <- cbind(levels(this.dat[,2]), stat.table)
					this.res <- c(paste(names(dataset)[i], "; Ref=", levels(this.dat[,2])[1], sep=""), 
								rep(NA, length(levels(this.dat[,1]))),
								as.character(round(this.res$statistic, digits=4)), as.character(round(this.res$p.value, digits=4)))
					out.table[nrow(out.table)+1, ] <- this.res
					out.table[(nrow(out.table)+1):(nrow(out.table)+(length(levels(this.dat[,2])))), 1:(1+length(levels(this.dat[,1])))] <- stat.table
				}
				print(names(dataset[i]))	
			}
		}
	}
	return(out.table)
}

#' Calculate numbers and statistics for table one
#'
#' @param g.obj A tableGrob object.
#' @param row row id.
#' @param col column id.
#' @param name default option
#' @return indexes of cells matching criteria.
#' 
find_cell <- function(g.obj, row, col, name="core-fg"){
	l <- g.obj$layout
	ind <- lapply(row, function(x) which(l$t == (x+1) & l$l==col & l$name==name))
	return(ind)
}

#' Plot table one
#'
#' @param table.obj A output from the tableone() function.
#' @param p.threshold A numeric indicating significant value cutoff for bolding p-value.
#' @return A plot of table one.
#' 
#' @import grid
#' @import gridExtra
#' @export
tableone.plot <- function(table.obj, p.threshold=0.05) {
	if(class(p.threshold) != "numeric") {
		stop("Please check the p-value threshold format...")
	} 

	table.obj[is.na(table.obj)] <- ""
	row.id <- which(as.numeric(table.obj$Pval)<p.threshold)
	col <- ncol(table.obj) + 1

	x <- hj <- matrix(c(0.9, rep(0.5, (ncol(table.obj)-1))), ncol=(col-1), nrow=nrow(table.obj), byrow=TRUE)
	tt1 <- ttheme_default(core=list(fg_params=list(hjust = as.vector(hj), x = as.vector(x))))
	g.obj <- tableGrob(table.obj, , rows = NULL, theme=tt1)

	ind <- find_cell(g.obj, row.id, col=(col-1), "core-fg")

	for (i in ind) {
		g.obj$grobs[i][[1]][["gp"]] <- gpar(fontface="bold")	
	}
	grid.draw(g.obj)
	grid.newpage()
}

#' Write table one to excel file
#'
#' @param table.obj A output from the tableone() function.
#' @param filename A character indicating the output name of the files. Default is table1.
#' @return A xlsx file of table one.
#' 
#' @import xlsx
#' @export
tableone.export <- function(table.obj, filename="Table1") {
	if(class(filename) != "character") {
		stop("Invalid file name...")
	}

	outputname <- paste(filename, ".xlsx", sep="")

	write.xlsx(table.obj, outputname, row.names = FALSE, append = TRUE)
}



# library(formattable)
# library("htmltools")
# library("webshot")  