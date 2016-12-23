all : compile
	stack exec hakyll-generate -- build

watch : compile
	stack exec hakyll-generate -- watch

clean-hakyll : compile
	stack exec hakyll-generate -- clean

compile :
	stack build
