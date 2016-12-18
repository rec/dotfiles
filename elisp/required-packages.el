(require 'package)

(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'cython-mode)
(require 'dash)
(require 'dash-functional)
(require 'git-gutter)
(require 'google-c-style)
(require 'guess-style)
(require 'markdown-mode)
(require 's)
(require 'saveplace)
(require 'uniquify)
(require 'yaml-mode)
