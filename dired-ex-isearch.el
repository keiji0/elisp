;; dired で Explorer のようにファイル名の 1 文字目で検索する
;; src: http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=dired%20explorer

(define-key dired-mode-map "/" 'dired-ex-isearch)

(defvar dired-ex-isearch-backspace "\C-h")
(defvar dired-ex-isearch-return "\C-g")

(defun dired-ex-isearch ()
  (interactive)
  (let ((REGEX1 "[0-9] ") (REGEX2 "[^ \n]+$")
        (FUNC 'dired-move-to-filename)
        (input (read-event))
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
            (throw 'END nil))
           ;; other command
           (t
            (setq unread-command-events (append (list input) unread-command-events))
            (throw 'END nil)))
          (funcall FUNC)
          (message str)
          (setq input (read-event)))))))
