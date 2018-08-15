#!/bin/sh

git config --global user.email "edujrrib@gmail.com"
git config --global user.name "Travis boot to build VIII Encontro website"

git clone -b gh-pages https://${GIT_KEY}@github.com/${TRAVIS_REPO_SLUG}.git dir-tmp
cd dir-tmp
rm -r ./*
cp -r ../site/public/* ./
git add --all *
git commit -m "Atualização automática (travis build ${TRAVIS_BUILD_NUMBER})" || true
git push origin gh-pages
