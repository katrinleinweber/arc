## delete any existing version of `ohicore`
for (p in c('ohicore')){
  if (p %in% rownames(installed.packages())){
    lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T)
    remove.packages(p, lib)
  }
}

## install dependencies
for (p in c('devtools', 'git2r')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  }
}

## install most current version of ohicore -- don't worry about the warnings. But make sure there are no errors.
devtools::install_github('ohi-science/ohicore@dev')