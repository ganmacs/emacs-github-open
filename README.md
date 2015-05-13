## Description

Open a commit url in github.com

## Requirement

* OSX

## Usage

If you use cask, add this line in `Cask` and `$ cask install`

```
(depends-on "github-commit-open" :git "https://github.com/ganmacs/emacs-github-open")
```

And Add this line in your `init.el`

```
(require 'github-commit-open)
(global-set-key (kbd "M-g o") 'github-commit-open)
```
