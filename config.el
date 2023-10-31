;; eslint
(setq flycheck-javascript-eslint-executable "eslint_d")

(add-hook 'typescript-tsx-mode-hook 'eslintd-fix-mode)
(add-hook 'typescript-mode-hook 'eslintd-fix-mode)
(add-hook 'web-mode-hook 'eslintd-fix-mode)

;; Set the default font and theme
(setq doom-theme 'doom-zenburn)
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 14))

;; Make code lines relative insteal of absolute
(setq display-line-numbers-type 'relative)

;; Org default directory override
(setq org-directory "~/.doom.d/org/")

;;Setup deft
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/.doom.d/org/")
(setq deft-recursive t)

(after! projectile
  (setq projectile-project-search-path '("~/repo/")))

;; This set emacs window size and opsition on startup
(setq initial-frame-alist
      '((width . 180)
        (height . 100)
        (top . 20)
        (left . 50)))

;; Web mode for react files
(use-package web-mode  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
	 ("\\.json\\'" . web-mode))
  :commands web-mode
  :config
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  )
