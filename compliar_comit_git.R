bookdown::render_book("index.Rmd", "bookdown::gitbook")

# Ejecutar git add
system("git add --all")

# Ejecutar git commit
system('git commit -m "Actualizar repositorio html"')

# Ejecutar git push
system("git push --verbose")

bookdown::render_book("index.Rmd", "bookdown::pdf_book")
