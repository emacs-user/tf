(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(blink-cursor-mode -1) ;; курсор не мигает
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Scrolling settings
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы  
(setq scroll-conservatively 10000)
;; Indent settings
(setq-default indent-tabs-mode nil) ;; отключить возможность ставить отступы TAB'ом
(setq-default tab-width          4) ;; ширина табуляции - 4 пробельных символа
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) ;; стандартная ширина отступа - 4 пробельных символа
(setq-default lisp-body-indent   4) ;; сдвигать Lisp-выражения на 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
(setq lisp-indent-function  'common-lisp-indent-function)
;; Linum plugin
(require 'linum)  ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк
;; Delete selection
(delete-selection-mode t)
;; Show-paren-mode settings
(show-paren-mode t) ;; включить выделение выражений между {},[],()
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()
;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a


(defun is-linux ()
  (string-equal system-type "gnu/linux"))
(defun is-windows ()
  (string-equal system-type "windows-nt"))
(defun is-osx ()
  (string-equal system-type "darwin"))

(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("myelpa" . "~/pro/packages/")))
(setq package-check-signature nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package telega
  :load-path  "~/.emacs.d/noelpa/telega.el"
  :commands (telega)
  :defer t
  :config
  (setq telega-chat-show-avatars nil
        telega-active-locations-show-avatars nil
        telega-company-username-show-avatars nil
        telega-root-show-avatars nil
        telega-user-show-avatars nil)
  (add-to-list 'exec-path "/usr/local/include")
  (add-to-list 'exec-path "/usr/local/lib"))

(use-package elpa-mirror
  :config
  (setq elpamr-default-output-directory "~/pro/packages/"))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode))

(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "<f5>") 'helm-M-x)
  (global-set-key (kbd "C-o") 'helm-find-files)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))


(use-package company
  :diminish company-mode
  :config
  (setq company-backends (remove 'company-ropemacs company-backends)
	    company-tooltip-limit 20
	    company-tooltip-align-annotations t)
  (global-company-mode 1))

(use-package yasnippet
  :init
  (add-to-list 'load-path
	       "~/.emacs.d/elpa/yasnippet-snippets-20200909.1058/snippets")
  :config
  (yas-global-mode 1))

(use-package magit)

(use-package bs
  :config
  (global-set-key (kbd "<f2>") 'bs-show))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package paren
  :config
  (show-paren-mode))

(use-package mu4e
  :ensure nil
  :init
  (if (is-osx)
      (progn
	(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.6.10/share/emacs/site-lisp/mu/mu4e")
	(add-to-list 'exec-path "/opt/homebrew/bin")
	(setq mu4e-mu-binary        "/opt/homebrew/bin/mu"
              mu4e-get-mail-command "/opt/homebrew/bin/mbsync -aq")))
  (if (is-linux)
      (progn
	(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
	(add-to-list 'exec-path "/usr/bin")
	(setq mu4e-mu-binary        "/usr/bin/mu"
              mu4e-get-mail-command "/usr/bin/mbsync -aq")))
  :config
  (global-set-key (kbd "<f9> m") 'mu4e)
  (setq mu4e-update-interval 600     
        mu4e-maildir       "~/pro/mail"
        mu4e-sent-folder   "/Отправленные"
        mu4e-trash-folder  "/Корзина"
        mu4e-drafts-folder "/Черновики"
        mu4e-refile-folder "/Архив"
        mu4e-show-images   t
        message-send-mail-function 'smtpmail-send-it     
        mu4e-html2text-command "html2text -utf8 -width 72"
        user-mail-address	"ievlev.sergey@gmail.com"
        user-full-name     "Сергей Иевлев"
        mu4e-compose-signature "С уважением,\nСергей Иевлев\n"
        mu4e-sent-messages-behavior  'delete
        smtpmail-stream-type nil
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-queue-mail t
        smtpmail-queue-dir "~/Maildir/Исходящие/cur"
        mu4e-headers-sort-direction 'ascending)
  (setq mu4e-bookmarks
        `( ,(make-mu4e-bookmark
             :name  "Не прочитанные"
             :query "flag:unread AND NOT maildir:/G/Спам"
             :key ?u)
           ,(make-mu4e-bookmark
             :name "Сегодня"
             :query "date:today..now AND NOT maildir:/G/Спам"
             :key ?t)
           ,(make-mu4e-bookmark
             :name "За последние 7 дней"
             :query "date:7d..now AND NOT maildir:/G/Спам"
             :key ?w)
           ,(make-mu4e-bookmark
             :name "С изображениями"
             :query "mime:image/* AND NOT maildir:/G/Спам"
             :key ?p)
           ,(make-mu4e-bookmark
             :name  "Не обработанные"
             :query "flag:unread AND maildir:/G/Входящие"
             :key ?b)))
  (setq mu4e-headers-fields
        '((:human-date    .  12)
          (:from          .  22)
          (:subject       .  nil)))
  (setq mu4e-headers-date-format "%Y-%m-%d"
        mu4e-headers-time-format "%H:%M")
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (require 'org-mu4e)
  (setq mu4e-org-support t
        mu4e-org-link-query-in-headers-mode nil)
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(use-package org
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c C-l") 'org-insert-link)
  (global-set-key (kbd "C-c C-o") 'org-open-at-point)
  (setq org-id-link-to-org-use-id t)
  (setq org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-tab-acts-natively t)
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
	   (R . t)
	   (python . t)
	   (ruby . t)
	   (shell . t)
	   (org . t)
	   (plantuml . t)
	   (latex . t)
	   (calc . t)
	   (gnuplot . t))))
  (turn-on-visual-line-mode)
  (org-indent-mode)
  (setq org-agenda-files (list "~/pro/org/"))
  (add-to-list 'org-emphasis-alist
	       '("=" (:foreground "red")))
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?D)
  (setq org-priority-default ?C)
  (setq org-plantuml-jar-path "~/.emacs.d/noelpa/plantuml-1.2021.16.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq org-todo-keywords
	(quote ((sequence "TODO(1)" "NEXT(2@/!)" "|" "DONE(3@/!)")
		(sequence "CONTROL(4@/!)" "MEET(5@/!)" "WAIT(6@/!)" "HOLD(7@/!)" "|" "CANCEL(8@/!)"))))
  (setq org-todo-keyword-faces
	(quote (("TODO"    :foreground "red"          :weight bold)
		("NEXT"    :foreground "blue"         :weight bold)
		("DONE"    :foreground "forest green" :weight bold)
		("CONTROL" :foreground "orange"       :weight bold)
		("WAIT"    :foreground "orange"       :weight bold)
		("HOLD"    :foreground "magenta"      :weight bold)
		("CANCEL"  :foreground "forest green" :weight bold)
		("MEET"    :foreground "blue"         :weight bold))))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (global-set-key (kbd "<f9> q") 'org-todo)
  (setq org-directory "~/pro/org")
  (setq org-default-notes-file "~/pro/org/inbox.org")
  (setq org-capture-templates
      (quote (("н" "Новая задача" entry (file "~/pro/org/inbox.org")
	       "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:CONTEXT: %a\n:END:\n:LOGBOOK:\n:END:" :clock-in t :clock-resume t)
	      ("з" "Заметка" entry (file "~/pro/org/inbox.org")
	       "* %?\n:PROPERTIES:\n:CREATED: %U\n:CONTEXT: %a\n:END:\n:LOGBOOK:\n:END:" :clock-in t :clock-resume t)
	      ("к" "Новый контакт" entry (file "~/pro/org/contacts.org")
	       "* %^{ФИО}%^{ORG}p%^{TITLE}p%^{PHONE}p%^{EMAIL}p")
	      ("в" "Встреча" entry (file "~/pro/org/inbox.org")
	       "* MEET %?\n:PROPERTIES:\n:CREATED: %U\n:CONTEXT: %a\n:END:\n:LOGBOOK:\n:END:" :clock-in t :clock-resume t)
	      ("т" "Звонок" entry (file "~/pro/org/inbox.org")
	       "* PHONE %?\n:PROPERTIES:\n:CREATED: %U\n:CONTEXT: %a\n:END:\n:LOGBOOK:\n:END:" :clock-in t :clock-resume t)
	      ("х" "Habit" entry (file "~/pro/org/inbox.org")
	       "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:CREATED: %U\n:CONTEXT: %a\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n:LOGBOOK:\n:END:"))))  
  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
  (setq org-use-property-inheritance t)
  
  (global-set-key (kbd "<f9> 1 c") 'tf-mu4e-get-task-from-1c-mail)
  
  (global-set-key (kbd "<f12>") 'org-agenda)

  (setq org-id-method (quote uuidgen))
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-refile-use-outline-path 'file)

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 20)))

  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
	(quote ((" " "Расписание"
		 ((agenda "" ((org-agenda-span 'day)))
		  (tags "вхд"
			((org-agenda-overriding-header "ВХОДЯЩИЕ")
			 (org-tags-match-list-sublevels nil)))
		  (alltodo ""
			   ((org-agenda-cmp-user-defined 'tf--agenda-sort)
			    (org-agenda-sorting-strategy '(user-defined-down))
			    (org-agenda-overriding-header "ПРИОРИТЕТЫ"))))))))

  (setq org-deadline-warning-days -1)

  (add-hook 'org-after-todo-state-change-hook 'tf--org-todo-state-change-property)

  (global-set-key (kbd "C-<f12>") 'tf-auto-task-scheduling)

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;;
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  
  (setq bh/keep-clock-running nil)

  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

  (require 'org-id)

  (setq org-time-stamp-rounding-minutes (quote (1 1)))
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
(setq org-columns-default-format "%80ITEM(Задача) %10Effort(Запланировано){:} %10CLOCKSUM(Потрачено)")
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00")
                                    ("STYLE_ALL" . "habit"))))
(setq org-agenda-log-mode-items (quote (closed state)))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
(setq org-log-reschedule nil)
(setq org-log-redeadline nil)
(setq org-log-refile 'time)

(setq org-log-note-headings
      '((done .        "%t: Задача закрыта")
        (state .       "%t: Статус изменен с %-9S на %-9s")
        (note .        "%t: Заметка")
        (reschedule .  "%t: Напоминание изменено с %S на %s")
        (delschedule . "%t: Напоминание %S удалено")
        (redeadline .  "%t: Срок изменен с %S на %s")
        (deldeadline . "%t: Срок %S удален")
        (refile .      "%t: Задача перемещена")
        (clock-out .   "")))

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)

(setq org-clock-out-remove-zero-time-clocks t)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(global-set-key (kbd "<f9> i") 'si/interrupt)


(define-key org-mode-map (kbd "<f9> t") 'si/org-set-tags)

(define-key org-mode-map (kbd "<f7>") 'org-narrow-to-subtree)
(define-key org-mode-map (kbd "C-<f7>") 'widen)

(define-key org-mode-map (kbd "<f9> n") 'org-add-note)

(define-key org-mode-map (kbd "<f9> p") 'org-set-property)

(define-key org-mode-map (kbd "<f9> a") 'org-attach-reveal)
(define-key org-mode-map (kbd "<f9> A") 'org-attach-reveal-in-emacs)
(setq org-attach-method 'mv)

(global-set-key (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c C-l") 'org-insert-link)
(define-key org-mode-map (kbd "C-c C-o") 'org-open-at-point)


(define-key org-mode-map (kbd "<f9> l") 'si/org-insert-extension-link)

(org-add-link-type "e1c" 'org-1c-open)


(global-set-key (kbd "<f9> f") 'si/helm-org-agenda-files-headings)

(global-set-key (kbd "<f9> F") 'org-search-view)

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-alphabetical-lists t)

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
(setq org-image-actual-width '(400))
(setq org-babel-results-keyword "results")
(setq org-confirm-babel-evaluate nil)

(when (eq system-type 'darwin)
  (global-set-key (kbd "s-<f9>") 'org-toggle-inline-images))

(when (eq system-type 'windows-nt)
  (global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)))

(use-package calendar
  :config
  (setq calendar-month-name-array
      ["Январь" "Февраль" "Март"     "Апрель"  "Май"    "Июнь"
       "Июль"   "Август"  "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"])
  (setq calendar-month-abbrev-array
      ["Янв" "Фев" "Мар" "Апр" "Май" "Июн"
       "Июл" "Авг" "Сен" "Окт" "Ноя" "Дек"])
  (setq calendar-day-name-array
	["Воскресенье" "Понедельник" "Вторник" "Среда" "Четверг" "Пятница" "Суббота"])
  (setq calendar-day-abbrev-array
	["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
  (setq calendar-week-start-day 1))

(use-package plantuml-mode
    :config
    (setq plantuml-jar-path "~/.emacs.d/noelpa/plantuml-1.2021.16.jar")
    (setq plantuml-default-exec-mode 'jar)
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package helm-org
  :config
    (global-set-key (kbd "<f9> <f9>") 'helm-org-capture-templates))

;(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command . helm-org-completing-read-tags))

(load-file "~/.emacs.d/noelpa/emacs-calfw/calfw.el")
(load-file "~/.emacs.d/noelpa/emacs-calfw/calfw-org.el")
(use-package calfw)
(use-package calfw-org
  :config
  (setq cfw:org-agenda-schedule-args '(:timestamp))
  (global-set-key (kbd "<f9> c") 'cfw:open-org-calendar))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/pro/wiki")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(use-package helm-org-contacts
  :ensure nil
  :init
  (load-file "~/.emacs.d/noelpa/org-contacts.el")
  (require 'org-contacts)
  (setq org-contacts-files '("~/pro/org/contacts.org"))
  (load-file "~/.emacs.d/noelpa/helm-org-contacts/helm-org-contacts.el")
  :config
  (global-set-key (kbd "<f9> k") 'helm-org-contacts))

(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line)))

(use-package auto-dim-other-buffers
    :init
    (auto-dim-other-buffers-mode t))
