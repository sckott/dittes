all: knit pdf word

knit:
	Rscript -e 'knitr::knit("data_summary.Rmd")'
	
pdf:
	Rscript -e 'rmarkdown::render("data_summary.Rmd", rmarkdown::pdf_document())'

word:
	Rscript -e 'rmarkdown::render("data_summary.Rmd", rmarkdown::word_document())'
