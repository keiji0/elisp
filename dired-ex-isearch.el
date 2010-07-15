;; dired-ex-isearch.el v0.1
;; This is free software
;;
;; * Base code
;; http://homepage1.nifty.com/blankspace/emacs/dired.html
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=dired%20explorer
;;
;; * Install
;;  (require 'dired-ex-isearch)
;;  (define-key dired-mode-map "/" 'dired-ex-isearch)

(defvar dired-ex-isearch-backspace "\C-h")
(defvar dired-ex-isearch-return "\C-g")
(defvar dired-ex-finish-hook nil)
(defvar dired-ex-move-hook nil)

(add-hook 'dired-ex-finish-hook
          (lambda ()
            (if (fboundp 'highline-local-off)
                (highline-local-off))))

(add-hook 'dired-ex-move-hook
          (lambda ()
            (if (fboundp 'highline-highlight-current-line)
                (highline-highlight-current-line))))

(defun dired-ex-isearch ()
  (interactive)
  (let ((REGEX1 "[0-9] ") (REGEX2 "[^ \n]+$")
        (input (read-quoted-char))
        (oldpoint (point)) regx str)
    (goto-char (point-min))
    (save-match-data
      (catch 'END
        (while t
          (cond
           ;; character
           ((and (integerp input)
                 (>= input ?!) (<= input ?~))
            (setq str (concat str (regexp-quote (char-to-string input))))
            (setq regx (concat REGEX1 str REGEX2))
            (beginning-of-line)
            (re-search-forward regx nil t nil))
           ;; backspace
           ((and (integerp input)
                 (or (eq 'backspace input)
                     (= input (string-to-char dired-ex-isearch-backspace))))
            (setq str (if (eq 0 (length str)) str (substring str 0 -1)))
            (setq regx (concat REGEX1 str REGEX2))
            (goto-char oldpoint)
            (re-search-forward regx nil t nil))
           ;; return
           ((and (integerp input)
                 (= input (string-to-char dired-ex-isearch-return)))
            (goto-char oldpoint)
            (message "return")
            (run-hooks 'dired-ex-finish-hook)
            (throw 'END nil))
           ;; other command
           (t
            (setq unread-command-events (append (list input) unread-command-events))
            (run-hooks 'dired-ex-finish-hook)
            (throw 'END nil)))
          (dired-move-to-filename)
          (run-hooks 'dired-ex-move-hook)
          (message "dired-ex: %s" str)
          (setq input (read-quoted-char)))))))

(provide 'dired-ex-isearch)
