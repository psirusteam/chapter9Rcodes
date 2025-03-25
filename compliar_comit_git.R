bookdown::render_book("index.Rmd", "bookdown::gitbook")

# Ejecutar git add
system("git add --all")

# Ejecutar git commit
system('git commit -m "Abstract reviewed"')

# Ejecutar git push
system("git push --verbose")

# Ejecutar git pull
system("git pull")

#bookdown::render_book("index.Rmd", "bookdown::pdf_book")
