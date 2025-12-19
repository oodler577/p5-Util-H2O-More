clean:
	dzil clean

test: clean
	dzil test

release: clean
	dzil build 
	mv -vf *.tar.gz ./releases/ 
	dzil clean
	git add ./releases/*.tar.gz
	git status
	@echo "tag and push ... "
