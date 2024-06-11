(require 'package)

(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; (require 'cython-mode)
(require 'dash)
(require 'dash-functional)
;; (require 'defaultcontent)
(require 'dired-x)
(require 'git-gutter)
;;(require 'google-c-style)
(require 'guess-style)
(require 'markdown-mode)
(require 's)
(require 'saveplace)
;; (require 'template)
;; (template-initialize)
(require 'uniquify)
(require 'yaml-mode)
