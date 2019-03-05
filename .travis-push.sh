#!/bin/sh

setup_git() {
	git config --global user.email "travis@travis-ci.org"
	git config --global user.name "Travis CI"
}

commit_readme() {
	git checkout master
	git status
	git add -f *.md
	git commit -m "Travis update: (Build $TRAVIS_BUILD_NUMBER)" -m "[skip ci]"
}

upload_files() {
	git remote rm origin
	git remote add origin https://dloewenstein:${GITHUB_TOKEN}@github.com/dloewenstein/ewavesPDFshiny.git > /dev/null 2>&1
	git push origin master --quiet > /dev/null 2>&1
}

setup_git
commit_readme

if [ $? -eq 0 ]; then
	echo "A new commit with changed *.md exists. Uploading to GitHub"
	upload_files
else
	echo "No changes to *.md. Nothing to do"
fi
