.SILENT:
.SUFFIX:
.PHONY : all clean

all: index.html index_bib.html

index.html index_bib.html: cv_rinderknecht.bib
	printf "Making index.html from cv_rinderknecht.bib..."
	bibtex2html -q -o index -s not_so_plain -d -r -t "Publications by Christian Rinderknecht" -nf ps PostScript -nf pdf PDF cv_rinderknecht.bib
	perl -pi -e "s|<h1>|<h2>|g;s|</h1>|</h2>|g" index.html
	echo " done."

clean:
	printf "Deleting index.html and index_bib.html..."
	\rm -f index.html index_bib.html
	echo " done."
