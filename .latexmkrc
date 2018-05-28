# -*-: mode perl -*-
# vi: set ft=perl :

$pdflatex = 'lualatex -interaction=nonstopmode -file-line-error -shell-escape -synctex=1 %O %S';
$bibtex = 'biber -u -U';
$pdf_previewer = '';
$pdf_mode = 1;

