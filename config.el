;; General
(setq user-full-name "Jean-Marc Prud'homme"
      user-mail-address "jm.prudhomme@icloud.com"
      doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 16)
      doom-theme 'doom-one
      display-line-numbers-type `relative
      load-prefer-newer t
      org-directory "~/.doom.d/org/"
      org-ellipsis " ▼ "
      org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
      org-agenda-start-day "+0d"
      org-agenda-span 1
      org-agenda-files '("~/.doom.d/org/tasks.org"
                         "~/.doom.d/org/agenda.org"))

(after! org
  (setq org-agenda-start-day "+0d"))

(setq org-archive-location (concat org-directory "archive/%s_archive::"))

;; This set emacs window size and opsition on startup
(setq initial-frame-alist
      '((width . 160)
        (height . 100)
        (top . 20)
        (left . 50)))

;; eslint
(setq flycheck-javascript-eslint-executable "eslint_d")

(add-hook 'typescript-tsx-mode-hook 'eslintd-fix-mode)
(add-hook 'typescript-mode-hook 'eslintd-fix-mode)
(add-hook 'web-mode-hook 'eslintd-fix-mode)

;;Setup deft
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/.doom.d/org/")
(setq deft-recursive t)

(after! projectile
  (setq projectile-project-search-path '("~/repo/")))


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

(org-super-agenda-mode)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(i)" "SOMEDAY(s)" "|" "DONE(d)" "BLOCKED(b)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#FB8500" :weight bold))
        ("DOING" . (:foreground "#FFB703" :weight bold))
        ("SOMEDAY" . (:foreground "#219EBC" :weight bold))
        ("DONE" . (:foreground "#70E000" :weight bold))
        ("BLOCKED" . (:foreground "#da2c38" :weight bold))
        ("CANCELLED" . (:foreground "#da2c38" :weight bold))))


(setq org-agenda-custom-commands
      '(("a" "Jm's Agenda"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "⏰ Schedule Today ⏰"
                          :time-grid t
                          :date today
                          :scheduled today
                          :deadline today
                          :order 1)
                         (:name "📅 Appointments soon 📅"
                          :tag "Appointment"
                          :order 2)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "🔥 Today's Tasks 🔥"
                           :tag "Today"
                           :order 4)
                          (:name "🚨 Important 🚨"
                           :tag "Important"
                           :priority "A"
                           :order 8)
                          (:name "🚓 Past Due 🚓"
                           :deadline past
                           :order 9)
                          (:name "🚓 Due Soon 🚓"
                           :and (:deadline future :not(:tag "Appointment"))
                           :order 16)
                          (:name "🚀 Projects 🚀"
                           :tag "Project"
                           :order 20)
                          (:name "💻 Emacs 💻"
                           :tag "Emacs"
                           :order 24)
                          (:name "📓 To read 📓"
                           :tag "Read"
                           :order 28)
                          (:name "🛍️ Shopping List 🛍️"
                           :tag "Shop"
                           :order 32)
                          (:name "❄️ trivial ❄️"
                           :priority<= "C"
                           :todo "SOMEDAY"
                           :tag ("Trivial" "Unimportant")
                           :order 90)
                          (:discard
                           (:tag "Appointment"))
                          ))))))))
(after! org
  (setq org-deadline-warning-days 30))
