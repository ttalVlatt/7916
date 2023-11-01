## Quarto .docx to .pdf converter
## h/t https://stackoverflow.com/questions/6011115/doc-to-pdf-using-python

## setup to work in quarto post-render
## https://quarto.org/docs/projects/scripts.html

## Requires MS Word installation on machine
## pip install docx2pdf (no conda installation)

from docx2pdf import convert
import os

output_dir = os.getenv("QUARTO_PROJECT_OUTPUT_DIR")

doc_docx = os.path.join(output_dir, "syllabus.docx")
doc_pdf = os.path.join(output_dir, "syllabus.pdf")

convert(doc_docx, doc_pdf)

## To work with multiple outputs for Quarto websites
## have Quarto create the docx and pdf (using LaTeX)
## This script will run after and overwrite the LaTeX .pdf 
## with the .pdf output from MS Word
