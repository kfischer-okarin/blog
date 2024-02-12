(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

; For generating diagrams
(package-install 'ob-mermaid)
