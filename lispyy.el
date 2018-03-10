;; (require 'cl-lib)
(require 'lispy)

(setq debug-on-error t)
;; (setq lispy-eval-display-style 'overlay)

;;;
;;;  Lisp syntax predicates
;;;

;;;;
;;;;  Primitive syntax predicates
;;;;

(defun lispyy-in-string-p (&optional pos)
  "Test if point is inside a string (corrected)."
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- "lispyy-in-string-p:")
    (msg-- (and (lispy--in-string-or-comment-p)
               (lispy--in-string-p)))))

(defun lispyy-in-comment-p (&optional pos)
  "Test if point is inside a comment (corrected)."
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- "lispyy-in-comment-p:")
    (msg-- (and (lispy--in-string-or-comment-p)
               (lispy--in-comment-p))))) ;  comment

(defun lispyy-in-string-or-comment-p (&optional pos)
  "TODO: documentation" ;;  comment
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- "lispyy-in-string-or-comment-p:")
    (msg-- (or (lispyy-in-string-p)
              (lispyy-in-comment-p)))))

;;;;
;;;;  String predicates
;;;;

(defun lispyy-before-string-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (not (lispyy-in-string-or-comment-p))
         (looking-at-p "[ \n\r\t]*\""))))

(defun lispyy-in-string-at-end-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (lispyy-in-string-p)
         (save-excursion
           (right-char)
           (not (lispyy-in-string-p))))))

(defun lispyy-after-string-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (not (lispyy-in-string-or-comment-p))
         (looking-back "\"[ \n\r\t]*" (point-min)))))

;;;;
;;;;  Comment predicates
;;;;

(defun lispyy-line-has-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (end-of-line)
    (lispyy-in-comment-p)))

(defun lispyy-within-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (and (lispyy-in-comment-p)
       (not (eolp))))

(defun lispyy-at-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (not (lispyy-in-comment-p))
         (looking-at-p ";")
         (save-excursion
           (right-char)
           (lispyy-in-comment-p)))))

(defun lispyy-at-or-in-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (or (lispyy-at-comment-p)
        (lispyy-in-comment-p))))

(defun lispyy-before-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (not (lispy--in-string-or-comment-p))
                (looking-at-p "[ \t]*;")))))

(defun lispyy-looking-at-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (not (lispy--in-string-or-comment-p))
                (looking-at-p "[ \t]*;")))))

(defun lispyy-looking-at-or-in-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (or (lispyy-looking-at-comment-p)
               (lispyy-in-comment-p)))))

(defun lispyy-before-or-in-comment-p (&optional pos)
  "TODO: documentation"
  (interactive "p")
  (save-excursion
    (if pos (goto-char pos))
    (or (lispyy-before-comment-p)
        (lispyy-in-comment-p))))

(defun lispyy-at-beginning-of-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (let ((pt (point)))
      (if (lispyy-beginning-of-comment)
          (= (point) pt)))))

(defun lispyy-before-beginning-of-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (let ((pt (point)))
      (if (lispyy-beginning-of-comment)
          (< pt (point))))))

(defun lispyy-after-beginning-of-comment-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (lispyy-in-comment-p)))

(defun lispyy-after-beginning-of-comment-text-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (let ((pt (point)))
      (if (lispyy-beginning-of-comment-text)
          (> pt (point))))))

(defun lispyy-comment-after-pos-p (pos)
  "TODO: documentation"
  (interactive)
  (msg-- (save-excursion
          (goto-char pos)
          (lispyy-end-of-code)
          (lispyy-before-comment-p))))

(defun lispyy-in-trailing-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (lispyy-in-comment-p)
         (lispyy-on-line-with-code-p))))

(defun lispyy-in-comment-after-code-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (lispyy-in-comment-p)
         (progn (beginning-of-line)
                (or (lispyy-in-string-p)
                    (not (looking-at-p "[ \t]*;")))))))

(defun lispyy-comment-after-list-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (lispy-forward 1)
    (lispyy-do-while 'right-char '(lambda () (looking-at-p " *)"))
                     'eolp (lambda ()))
    (msg-- (lispyy-before-comment-p))))

(defun lispyy-line-has-trailing-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (progn (beginning-of-line)
                (not (lispyy-before-comment-p)))
         (progn (end-of-line)
                (lispyy-in-comment-p)))))

(defun lispyy-eol-or-before-or-in-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (or (lispyy-eolp)
        (lispyy-before-or-in-comment-p))))

(defun lispyy-on-whole-line-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (beginning-of-line)
    (and (not (lispyy-in-string-p))
         (looking-at-p "[ \t]*;"))))

;;;;
;;;;  Code predicates
;;;;

(defun lispyy-on-line-with-code-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (or (progn (beginning-of-line)
               (lispyy-in-string-p))
        (and (not (lispyy-line-empty-p))
             (progn (back-to-indentation)
                    (not (looking-at ";")))))))

;; (define-key lispy-mode-map (kbd "s-x") 'lispyy-on-line-with-code-p)
;; (define-key lispy-mode-map (kbd "s-y") (lambda () (interactive) (msg (looking-back-on-line "[^ ] *"))))

;;  FIXME
(defun lispyy-line-has-code-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (end-of-line)
    (if (lispyy-line-has-comment-p)
        (lispyy-beginning-of-comment))   ;;  FIXME here
    (msg-- "lispyy-line-has-code-p:")
    (msg-- (not (lispyy-bolp)))))

(defun lispyy-line-has-code-only-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (if (lispyy-line-has-code-p)
      (not (lispyy-line-has-comment-p))))

(defun lispyy-next-line-has-code-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (forward-line)
    (lispyy-on-line-with-code-p)))

(defun lispyy-code-to-left-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (msg (if (not (lispyy-in-comment-p))
           (or (and (lispyy-in-string-p)
                    (not (bolp)))
               (looking-back-on-line "[^ \t][ \t]*")))))

(defun lispyy-code-to-right-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (msg (if (not (lispyy-in-comment-p))
           (or (and (lispyy-in-string-p)
                    (not (eolp)))
               (looking-forward-on-line "[ \t]*[^ \t;\n\r]")))))

'(global-set-key (kbd "M-1") 'lispyy-code-to-left-p)   ;;  test
'(global-set-key (kbd "M-2") 'lispyy-code-to-right-p)
'(global-set-key (kbd "M-3") (lambda () (interactive) (kill-line 1))) ;;  test

(defun lispyy-before-code-p ()
  "TODO: documentation"
  (interactive)
  (msg (and (lispyy-bolp)
            (lispyy-code-to-right-p))))

;; (defun lispyy-after-code-p ()
;;   "TODO: documentation"
;;   (interactive)
;;   (msg (and (lispyy-code-to-left-p)
;;             (or (lispyy-eolp)
;;                 (lispyy-before-comment-p)))))

(defun lispyy-eocp (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (let ((pt (point)))
      (lispyy-end-of-code)
      (= (point) pt))))

;; (defun lispyy-on-line-with-code-p (&optional pos)   ;;  test
;;   "TODO: documentation"
;;   (interactive)
;;   (msg (save-excursion
;;          (if pos (goto-char pos))
;;          (if (lispyy-line-has-comment-p)
;;              (lispyy-beginning-of-comment)
;;            (end-of-line))
;;          (msg-- (looking-back-on-line "[^ ] *")))))

(defun lispyy-past-code-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (lispyy-on-line-with-code-p)
         (let ((pt (point)))
           ;; (lispyy-end-of-code)
           ;; (msg-- (>= pt (point)))
           (msg-- (>= pt (lispyy-end-of-code)))))))

(defun lispyy-after-beginning-of-code-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (if (lispyy-on-line-with-code-p)
        (let ((pt (point)))
          (back-to-indentation)
          (> pt (point))))))

(defun lispyy-before-end-of-code-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (msg-- (save-excursion
          (if pos (goto-char pos))
          ;; (if (lispyy-in-comment-p)
          ;;     (lispyy-beginning-of-comment))
          ;; (looking-back-on-line "[^ ]")
          (if (lispyy-on-line-with-code-p)
              (not (lispyy-past-code-p))))))

;; (define-key lispy-mode-map (kbd "s-x") 'lispyy-before-end-of-code-p)

(defun lispyy-after-code-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (lispyy-past-code-p)
               (not (lispyy-in-comment-p))))))

(defun lispyy-after-code-with-no-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (lispyy-after-code-p)
               (lispyy-eolp)))))   ;  abc

(defun lispyy-after-code-with-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (lispyy-after-code-p)
               (lispyy-line-has-comment-p)))))   ;  abc

(defun lispyy-after-code-before-comment-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (lispyy-after-code-p)
               (lispyy-before-comment-p)))))

;;;;
;;;;  List predicates
;;;;

(defun lispyy-beginning-of-list-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (if (not (lispyy-in-string-or-comment-p))
        ;; (msg-- (looking-at-p "'?("))
        (msg-- (looking-at-p "(")))))

(defun lispyy-at-trailing-closing-parens-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (not (lispy--in-string-or-comment-p))
         ;; ;; (looking-back-on-line ") *")
         ;; (looking-back-on-line ") *")
         ;; ;; (looking-at-p " *)[ )]*;")
         (looking-at-p "[) ]*;\\|[) ]*$"))))

;;;;
;;;;  Line predicates
;;;;

(defun lispyy-bolp (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (looking-back-on-line "^[ \t]*")))

(defun lispyy-eolp (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (looking-at-p "[ \t]*$")))

(defun lispyy-before-end-of-line-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (not (eolp))))

(defun lispyy-after-beginning-of-line-p (&optional pos)   ;;  test
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (not (bolp))))

(defun lispyy-line-empty-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (lispyy-bolp)
         (lispyy-eolp)
         ;; (not (lispyy-in-string-p))
         )))

;; (defun current-line-empty-p (&optional pos)
;;   (save-excursion
;;     (if pos (goto-char pos))
;;     (beginning-of-line)
;;     (looking-at-p "[[:space:]]*$")))

;;;;
;;;;  lipsyy predicates
;;;;

(defun lispyy-left-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (not (lispyy-in-string-or-comment-p))
               (lispy-left-p)))))

(defun lispyy-right-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (not (lispyy-in-string-or-comment-p))
               (lispy-right-p)))))

(defun lispyy-left-q (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (lispyy-before-string-p)
               (looking-at-p "\"")))))

(defun lispyy-right-q (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (and (lispyy-after-string-p)
               (looking-back "\"" (1- (point)))))))

(defun lispyy-lispy-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (or (lispyy-left-p)
              (lispyy-right-p)))))

(defun lispyy-lispy-q (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (or (lispyy-left-q)
              (lispyy-right-q)))))

(defun lispyy-special-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (msg-- (or (lispyy-lispy-p)
              (lispyy-lispy-q)
              (bolp)))))

(defun lispyy-after-lispy-left-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (not (lispyy-in-string-or-comment-p))
         (looking-back-on-line "( *"))))

(defun lispyy-between-lispy-left-right-p (&optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (and (not (lispyy-in-string-or-comment-p))
         (looking-at-p " *)")
         (looking-back-on-line "( *"))))

(defun lispyy-flow-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (lispy-flow 1)))

(defun lispyy-different-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (lispy-different)))

;;;;
;;;;  Meta-predicates
;;;;

(defun position-satisfies-p (pred &optional pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (apply pred)))

(defun position-of (command)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (call-interactively command)
    (point)))

;;;
;;;  Positions and bounds
;;;

(defun lispyy-end-of-line-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion                       ; asdfasd
    (end-of-line)
    (msg-- (point))))

(defun lispyy-line-of-code-end-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion                       ; asdfasd
    (lispyy-end-of-code)
    (msg-- (point))))

(defun lispyy-bounds-string ()
  "TODO: documentation"
  (interactive)
  (msg-- (lispy--bounds-string)))

(defun lispyy-beginning-of-string-position ()
  "TODO: documentation"
  (interactive)
  (msg (car (lispy--bounds-string))))

(defun lispyy-end-of-string-position ()
  "TODO: documentation"
  (interactive)
  (msg (cdr (lispy--bounds-string))))

(defun lispyy-bounds-list ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if (lispy-right-p) (left-char))
    (msg-- (lispy--bounds-list))))

'(lispyy-bounds-list)
'(quote (lispy--bounds-list))

;; (defun test1 (line column)
;;   "TODO: documentation"
;;   (interactive)
;;   (while (<= 1 2)
;;     (move-to-column column t)
;;     ;; (asd
;;     ;;
;;     ;; (if (eolp)
;;     ;;     (insert prefix)
;;     ;;   (insert (concat prefix " ")))
;;     ;; (if (eolp)
;;     ;;     (insert prefix)
;;     ;;   (insert (concat prefix " ")))
;;     ;; (if (eolp)
;;     ;;     (insert prefix)
;;     ;;   (insert (concat prefix " ")))
;;     (forward-line)
;;     (setq line (1+ line))))

;;
;; (let ((current-prefix-arg '(4)))
;;   (call-interactively 'lispy-comment))

;;;
;;;  looking-at functions
;;;

(defun looking-forward-on-line (regexp)
  "TODO: documentation"
  (interactive)
  (looking-at regexp))

(defun looking-back-on-line (regexp)
  "TODO: documentation"
  (interactive)
  (looking-back regexp (line-beginning-position)))

;; (defun test ()
;;   "TODO: documentation"
;;   (let ((buffer-undo-list nil)
;;         kill-ring
;;         kill-ring-yank-pointer)
;;     (kill-line)
;;     (kill-line)
;;     (undo)))

;; (defun lispyy-pred-at-pos ()
;;   "TODO: documentation"
;;   (interactive)
;;   )

(defun lispyy-last-line-number-of-list ()
  "TODO: documentation"
  (interactive)
  (let ((pt (point))
        last-line)
    (lispy-forward 1)
    (let ((last-line-number (line-number-at-pos)))
      (goto-char pt)
      last-line-number)))

(quote
 ((defvar comment/line/column (cons "; zero" (cons 1 2)))
  (car  comment/line/column)
  (cadr comment/line/column)
  (cddr comment/line/column)))

(defun lispyy-extent-of-last-sexp (&optional limit)
  "TODO: documentation"
  (interactive)
  "     ")

;; (defun ws-test ()
;;   "TODO: documentation"
;;   (interactive)
;;   ;; (msg-- (looking-at-p "[:space:]*("))
;;   (msg-- (looking-at-p "[ \n\r\t]*(")))

;; (defun looking-at-test ()
;;   "TODO: documentation"
;;   (interactive)
;;   (msg (looking-at ";+ \\|;+$")))

(quote
 ((
   ())
  ()))

(defun lispyy-nop ()
  "TODO: documentation"
  (interactive))

;;;
;;;  Control statements
;;;

(defun lispyy-do-while (action condition &optional break-condition break-action)
  "TODO: documentation"
  (message- "lispyy-do-while: %S" break-condition)
  (while (and (if break-condition
                  (not (funcall break-condition))
                t)
              (funcall condition))
    (call-interactively action))

  (if (and break-condition
           (funcall break-condition))
      (and break-action
           (funcall break-action))))

'(global-set-key (kbd "M-1") 'lispyy-save-trailing-comment)      ;  comment 1
'(global-set-key (kbd "M-2") 'lispyy-restore-trailing-comment)   ;  comment 2

'(global-set-key (kbd "M-1") 'lispy--in-string-p)   ;  comment 1
'(global-set-key (kbd "M-2") 'lispyy-in-string-p)   ;  comment 2

;;  http://stackoverflow.com/questions/13141292/how-to-remove-the-top-entry-pop-from-the-emacs-kill-ring
'(pop kill-ring)

;; (global-set-key (kbd "M-t") 'lispyy-end-of-line)

(defun lispyy-do-while-still (action condition &optional break-condition break-action)
  "TODO: documentation"
  (while (and (if break-condition
                  (not (funcall break-condition))
                t)
              (save-excursion
                (call-interactively action)
                (funcall condition)))
    (call-interactively action))
  (if (and break-condition
           (funcall break-condition))
      (and break-action
           (funcall break-action))))

;; (defun test (&optional arg)
;;   "TODO: documentation"
;;   (interactive "P")
;;   (msg-- (lispyy-test)))

;; (defun lispyy-test ()
;;   "TODO: documentation"
;;   (lispy-kill)
;;   (append-next-kill)
;;   (lispy-kill))

(defun lispyy-do-until (action condition &optional break-condition break-action)
  "TODO: documentation"
  (while (and (if break-condition
                  (not (funcall break-condition))
                t)
              (progn (call-interactively action)
                     (not (funcall condition)))))
  (if (and break-condition
           (funcall break-condition))
      (and break-action
           (funcall break-action))))

;; (defun save-editing-state (&rest commands)
;;   "TODO: documentation"
;;   (interactive)
;;   (save-mark-and-excursion
;;    (save-restriction
;;      (save-match-data
;;        (let ((buffer-undo-list nil)
;;              kill-ring
;;              kill-ring-yank-pointer
;;              (result (eval commands)))
;;          (undo)
;;          result)))))

;; (defmacro with-cloned-buffer-0 (&rest body)
;;   "Executes BODY just like `progn'."
;;   `(let (buffer-undo-list
;;          (message-log-max nil)
;;          (inhibit-message t)
;;          ;; (temp-buffer (clone-indirect-buffer nil nil))
;;          (temp-buffer (clone-indirect-buffer nil t)))
;;      ;; (with-current-buffer temp-buffer
;;      ;;   ;; (add-to-list 'warning-suppress-types '(undo))
;;      ;;   (let ((buffer-undo-list nil)
;;      ;;         (message-log-max nil)
;;      ;;         (inhibit-message t))
;;      ;;     ,@body
;;      ;;     (undo)))
;;      (let ((buffer-undo-list nil)
;;            (message-log-max nil)
;;            (inhibit-message t))
;;        ,@body
;;        ;; (undo)
;;        (primitive-undo 1 buffer-undo-list))
;;      ;; (kill-buffer temp-buffer)
;;      (kill-buffer-and-window)))

;; (defmacro with-cloned-buffer-1 (&rest body)
;;   "Executes BODY just like `progn'."
;;   `(let (buffer-undo-list
;;          (message-log-max nil)
;;          (inhibit-message t)
;;          (temp-buffer (clone-indirect-buffer nil t)))
;;      (let ((buffer-undo-list nil)
;;            (message-log-max nil)
;;            (inhibit-message t)
;;            z)
;;        (setq z (progn ,@body))
;;        (primitive-undo 1 buffer-undo-list)
;;        (kill-buffer-and-window)
;;        z)))

;; (defmacro with-cloned-buffer (&rest body)
;;   "Executes BODY just like `progn'."
;;   `(let (buffer-undo-list)
;;      (clone-indirect-buffer nil t)
;;      (let ((buffer-undo-list nil)
;;            z)
;;        (setq z (progn ,@body))
;;        (primitive-undo 1 buffer-undo-list)
;;        (kill-buffer-and-window)
;;        z)))

;; (defmacro with-cloned-buffer (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let (buffer-undo-list)   ;  maintain original buffer-undo-list
;;        (clone-indirect-buffer nil t)
;;        (let (,return-value)
;;          (setq ,return-value (progn ,@body))
;;          (primitive-undo 1 buffer-undo-list)
;;          (kill-buffer-and-window)
;;          ,return-value))))

;; (defmacro with-cloned-indirect-buffer (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let (buffer-undo-list)   ;  maintain original buffer-undo-list
;;        (clone-indirect-buffer nil t)
;;        (let ((,return-value (progn ,@body)))
;;          (primitive-undo 1 buffer-undo-list)
;;          (kill-buffer-and-window)
;;          ,return-value))))

;; (defmacro with-cloned-indirect-buffer (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let (buffer-undo-list      ;  maintain original buffer-undo-list
;;            (indirect-buffer (clone-indirect-buffer nil nil)))
;;        (let ((,return-value (progn ,@body)))
;;          (primitive-undo 1 buffer-undo-list)
;;          (kill-buffer indirect-buffer)
;;          ,return-value))))

(defmacro with-cloned-indirect-buffer (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let (buffer-undo-list   ;  maintain original buffer-undo-list
           (indirect-buffer (clone-indirect-buffer nil nil)))
       (let ((,return-value (progn ,@body)))
         (primitive-undo 1 buffer-undo-list)
         (kill-buffer indirect-buffer)
         ,return-value))))

(defmacro save-buffer-state (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  `(let (buffer-undo-list   ;  maintain original buffer-undo-list
         (indirect-buffer (clone-indirect-buffer nil nil)))
     (unwind-protect
         (progn ,@body)
       (primitive-undo 1 buffer-undo-list)
       (kill-buffer indirect-buffer))))

'(defmacro save-buffer-state* (&rest body)
   "Executes BODY just like `progn' but maintains original buffer state."
   (declare (indent 0))
   (let ((variable-symbol-list (mapcar (lambda (x) (list x x)) (seq-remove (lambda (x) (or (equal x 'body) (equal x 'x))) (variable-symbol-list)))))
     `(let ,variable-symbol-list
        (let (buffer-undo-list   ;  maintain original buffer-undo-list
              (indirect-buffer (clone-indirect-buffer nil nil)))
          (unwind-protect
              (progn ,@body)
            (primitive-undo 1 buffer-undo-list)
            (kill-buffer indirect-buffer))))))

(setq variable-symbol-list 1)
'(macroexpand-1 '(save-buffer-state* (insert "A")))

'(macroexpand-1
  '(save-buffer-state*
     (insert "A")
     (left-char 1)
     (looking-at-p "A")))

'(save-buffer-state*
   (insert "A")
   (left-char 1)
   (looking-at-p "A"))

(defun test (arg)
  "TODO: documentation"
  (interactive)
  (let (arg)
    ;; (setq arg "setq")
    (msg arg))
  (msg arg))

'(test "abc")

'(setq test "abc")

'(defun test2 ()
  "TODO: documentation"
  (interactive)
  (let ((test test))
    (test test)))

'(test test)
'(test2)

;; '(defmacro save-buffer-state** (&rest body)
;;    "Executes BODY just like `progn' but maintains original buffer state."
;;    (declare (indent 0))
;;    (let ((variable-symbol-list (mapcar (lambda (x) (list x x)) (seq-remove (lambda (x) (or (equal x 'body) (equal x 'x))) (variable-symbol-list))))
;;          (function-symbol-list (mapcar (lambda (x) (list 'symbol-function x)) (function-symbol-list))))
;;      `(let ,variable-symbol-list
;;         (letf ,function-symbol-list
;;           (let (buffer-undo-list ;  maintain original buffer-undo-list
;;                 (indirect-buffer (clone-indirect-buffer nil nil)))
;;             (unwind-protect
;;                 (progn ,@body)
;;               (primitive-undo 1 buffer-undo-list)
;;               (kill-buffer indirect-buffer)))))))

;; (defmacro save-buffer-state** (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((protected-variable-symbol-pairs-list (mapcar (lambda (x) (list x x)) (seq-remove (lambda (x) (or (equal x 'body) (equal x 'x))) '(protected-variable-symbol-list))))
;;         `(let ,protected-variable-symbol-pairs-list
;;            (let (buffer-undo-list    ;  maintain original buffer-undo-list
;;                  (indirect-buffer (clone-indirect-buffer nil nil)))
;;              (unwind-protect
;;                  (progn ,@body)
;;                (primitive-undo 1 buffer-undo-list)
;;                (kill-buffer indirect-buffer)))))))

(setq body '((protect a b c) (insert "A") (left-char 1) (looking-at-p "A")))
(if (eq (caar body) 'protect)
    (cdar body))

;; (defmacro save-buffer-state** (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let* ((protected-variable-symbol-list (if (eq (caar body) 'protect)
;;                                              (cdar body)))
;;          (body (if (eq (caar body) 'protect)
;;                    (cdr body)
;;                  body))
;;          (protected-let-symbol-valueform-list (mapcar #'make-let-symbol-valueform protected-variable-symbol-list)))
;;     `(let ,protected-let-symbol-valueform-list
;;        (let (buffer-undo-list    ;  maintain original buffer-undo-list
;;              (indirect-buffer (clone-indirect-buffer nil nil)))
;;          (unwind-protect
;;              (progn ,@body)
;;            (primitive-undo 1 buffer-undo-list)
;;            (kill-buffer indirect-buffer))))))
;;
;; (macroexpand-1
;;  '(save-buffer-state**
;;     (protect x y z)
;;     (insert "A")
;;     (left-char 1)
;;     (looking-at-p "A")))
;;
;; (progn (setq x 3)
;;        (save-buffer-state**
;;          (protect x y z)
;;          (setq x 4)
;;          (insert "A")
;;          (left-char 1)
;;          (looking-at-p "A")
;;          (msg- x))
;;        (msg- x))

(defmacro protect-buffer-state (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let* ((protected-variable-symbol-list      (get-protected-variable-symbol-list body))
         (protected-let-symbol-valueform-list (make-let-symbol-valueform-list protected-variable-symbol-list))
         (body                                (filter-protect-form-from-body body))
         (indirect-buffer                     (cl-gensym "indirect-buffer")))
    `(let ,protected-let-symbol-valueform-list
       (let (buffer-undo-list    ;  maintain original buffer-undo-list
             (,indirect-buffer (clone-indirect-buffer nil nil)))
         (unwind-protect
             (progn ,@body)
           (primitive-undo 1 buffer-undo-list)
           (kill-buffer ,indirect-buffer))))))

'(macroexpand-1
  '(protect-buffer-state
     (protect kill-whole-line)
     (setq kill-whole-line nil)
     (kill-line)))

'(protect-buffer-state
   (protect kill-whole-line)
   (set kill-whole-line nil)
   (kill-line))

'(macroexpand-1
  '(protect-buffer-state
     (protect x y z)
     (insert "A")
     (left-char 1)
     (looking-at-p "A")))

'(progn (setq x 3)
        (save-buffer-state**
         (protect x y z)
         (setq x 4)
         (insert "A")
         (left-char 1)
         (looking-at-p "A")
         (msg- x))
        (msg- x))

;; (gensym "indirect-buffer")

(defun get-protected-variable-symbol-list (body)
  "TODO: documentation"
  (interactive)
  (if (eq (caar body) 'protect)
      (cdar body)))

(defun filter-protect-form-from-body (body)
  "TODO: documentation"
  (interactive)
  (if (eq (caar body) 'protect)
      (cdr body)
    body))

(defun make-let-symbol-valueform (symbol)
  "TODO: documentation"
  (interactive)
  (list symbol (list 'if (list 'boundp (list 'quote symbol))
                     symbol
                     nil)))

(defun make-let-symbol-valueform-list (symbol-list)
  "TODO: documentation"
  (interactive)
  (mapcar #'make-let-symbol-valueform symbol-list))

'(with-cloned-indirect-buffer
   (insert "A")
   (left-char 1)
   (looking-at-p "A"))

'(with-cloned-indirect-buffer
   (insert "A")
   (left-char 1)
   (looking-at-p "B"))

'(with-cloned-indirect-buffer
   (let ((kill-whole-line nil))
     (kill-line)
     (eolp)))

'(with-cloned-indirect-buffer
   (let ((kill-whole-line t))
     (kill-line)
     (eolp)))

'(let ((buffer-file-name nil))
   (clone-buffer nil t))

(defmacro with-cloned-buffer (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let ((buffer-file-name nil))
       (clone-buffer nil t)
       (let ((,return-value (progn ,@body)))
         (kill-buffer-and-window)
         ,return-value))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 (insert "A")
                                 (left-char 1)
                                 (looking-at-p "A"))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 (insert "A")
                                 (left-char 1)
                                 (looking-at-p "B"))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (with-cloned-buffer
                                      (let ((kill-whole-line nil))
                                        (kill-line)
                                        (eolp))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (with-cloned-buffer
                                      (let ((kill-whole-line t))
                                        (kill-line)
                                        (eolp))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 nil)))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 (msg (setq god-local-mode (not god-local-mode))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (save-buffer-state
                                 (msg (setq god-local-mode (not god-local-mode))))))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(catch 'done
;;        (atomic-change-group
;;          ;; (clone-indirect-buffer nil t)
;;          (let ((,return-value (progn ,@body)))
;;            ;; (kill-buffer-and-window)
;;            (throw 'done ,return-value))))))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let ((,return-value (progn
;;                             (clone-indirect-buffer nil t)
;;                             (catch 'done
;;                               (atomic-change-group
;;                                 (let ((,return-value (progn ,@body)))
;;                                   (throw 'done ,return-value)))))))
;;        (kill-buffer-and-window)
;;        ,return-value)))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let ((,return-value (unwind-protect
;;                               (progn
;;                                 (clone-indirect-buffer nil t)
;;                                 (catch 'done
;;                                   (atomic-change-group
;;                                     (let ((,return-value (progn ,@body)))
;;                                       (throw 'done ,return-value)))))
;;                             (kill-buffer-and-window))))
;;        (msg ,return-value))))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let ((,return-value (unwind-protect
;;                               (catch 'done
;;                                 (clone-indirect-buffer nil t)
;;                                 (atomic-change-group
;;                                   (throw 'done (progn ,@body))))
;;                             (kill-buffer-and-window))))
;;        ,return-value)))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   `(unwind-protect
;;        (catch 'done
;;          (clone-indirect-buffer nil t)
;;          (atomic-change-group
;;            (throw 'done (progn ,@body))))
;;      (kill-buffer-and-window)))

(defmacro save-buffer-state-2 (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let (point
           (indirect-buffer (clone-indirect-buffer nil nil)))
       (unwind-protect
           (with-current-buffer indirect-buffer
             (catch 'done
               (atomic-change-group
                 (let ((,return-value (progn ,@body)))
                   (throw 'done ,return-value)))))
         (kill-buffer indirect-buffer)))))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((variable-symbol-list '(variable-symbol-list))
;;         (function-symbol-list '(function-symbol-list)))
;;     `(let ,variable-symbol-list
;;        (letf ,function-symbol-list
;;          (unwind-protect
;;              (catch 'done
;;                (clone-indirect-buffer nil t)
;;                (atomic-change-group
;;                  (throw 'done (progn ,@body))))
;;            (kill-buffer-and-window))))))

'(macroexpand-1 '(save-buffer-state nil))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (save-buffer-state
                                 (msg (setq god-local-mode (not god-local-mode))))))

(global-set-key (kbd "M-1") (lambda ()
                              (interactive)
                              (msg (save-buffer-state-2
                                     (insert "A")
                                     (left-char 1)
                                     (looking-at-p "A")))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (save-buffer-state
                                      (insert "A")
                                      (left-char 1)
                                      (looking-at-p "B")))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (save-buffer-state-2
                                      (let ((kill-whole-line nil))
                                        (kill-line)
                                        (eolp))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (save-buffer-state-2
                                      (let ((kill-whole-line t))
                                        (kill-line)
                                        (eolp))))))

'(loop for x being the symbols if (boundp x) collect (symbol-name x))

;; (defun bound-symbol-list ()
;;   "TODO: documentation"
;;   (interactive)
;;   (loop for x being the symbols if (boundp x) collect x))

(defun variable-symbol-list ()
  "TODO: documentation"
  (interactive)
  (loop for x being the symbols
        if (and (boundp x) (not (keywordp x)) (not (eq x nil)))
        collect x))

(defun function-symbol-list ()
  "TODO: documentation"
  (interactive)
  (loop for x being the symbols
        if (fboundp x)
        collect x))

'(benchmark-run (variable-symbol-list))
'(benchmark-run (function-symbol-list))

;; (defmacro with-indirect-buffer-in-foo-mode (beg end &rest body)
;;   `(with-current-buffer (clone-indirect-buffer nil nil)
;;      (Narrow-to-region beg end)
;;      (foo-mode)
;;      (unwind-protect
;;          ,body
;;        (kill-buffer (current-buffer)))))

;; (defmacro with-indirect-buffer (&rest body)
;;   `(with-current-buffer (clone-indirect-buffer nil nil)
;;      ;; (narrow-to-region beg end)
;;      ;; (foo-mode)
;;      (unwind-protect
;;          @,body
;;        (kill-buffer (current-buffer)))))

;; (with-indirect-buffer
;;  (let ((kill-whole-line t))
;;    (kill-line)
;;    (eolp)))

'(clone-indirect-buffer nil t)
'(setq kill-whole-line nil)
'(setq kill-whole-line t)

;; (with-cloned-buffer
;;   (let ((kill-whole-line t))
;;     (kill-line)
;;     (eolp)))

;; (with-cloned-buffer
;;   (insert "A")
;;   (left-char 1)
;;   (looking-at-p "A"))

;; (with-cloned-buffer
;;   (insert "A")
;;   (left-char 1)
;;   (looking-at-p "B"))

;; (with-cloned-buffer
;;   (insert "A")
;;   (insert "B")
;;   (left-char 2)
;;   (looking-at-p "B"))

;; (let ((temp-buffer (clone-indirect-buffer nil nil)))
;;   (with-current-buffer temp-buffer
;;     (insert "A")))

;;;
;;;  Movement
;;;

;;;;
;;;;  Beginning/end of comments
;;;;

(defun lispyy-beginning-of-comment ()   ;;  test
  "TODO: documentation"
  (interactive)
  (when (lispyy-line-has-comment-p)
    (end-of-line)
    (lispyy-do-until 'left-char 'lispyy-at-comment-p)
    t))

(defun lispyy-beginning-of-comment-text ()
  "TODO: documentation"
  (interactive)
  (when (lispyy-line-has-comment-p)
    (lispyy-beginning-of-comment)
    (lispyy-do-until 'right-char (lambda () (looking-at-p "[^;]"))
                     'eolp)
    (lispyy-do-until 'right-char (lambda () (looking-at-p "[^ \t]"))
                     'eolp)   ;;
    t))                       ;  comment 2

(defun lispyy-forward-to-beginning-of-comment ()   ;;  test
  "TODO: documentation"
  (interactive)
  (if (lispyy-before-beginning-of-comment-p)
      (lispyy-beginning-of-comment)))

(defun lispyy-backward-to-beginning-of-comment ()   ;;  test
  "TODO: documentation"
  (interactive)
  (if (lispyy-after-beginning-of-comment-p)   ;;  test
      (lispyy-beginning-of-comment))
  ;; (if (lispyy-in-comment-p)
  ;;     (lispyy-beginning-of-comment))
  )

(defun lispyy-backward-to-beginning-of-comment-text ()   ;;  test
  "TODO: documentation"
  (interactive)
  (if (lispyy-after-beginning-of-comment-text-p)
      (lispyy-beginning-of-comment-text)))

;;;;
;;;;  Beginning/end of code
;;;;

(defun lispyy-beginning-of-code ()
  "TODO: documentation"
  (interactive)
  (when (lispyy-line-has-code-p)
    (back-to-indentation)
    t))

(defun lispyy-end-of-code ()   ;;  test
  "TODO: documentation"
  (interactive)
  (if (lispyy-line-has-code-p)
      (progn
        (end-of-line)
        (lispyy-do-while-still 'left-char 'lispyy-eol-or-before-or-in-comment-p
                               'bolp)
        (point))))

(defun lispyy-backward-to-beginning-of-code ()   ;;  test
  "TODO: documentation"
  (interactive)
  (if (lispyy-after-beginning-of-code-p)
      (lispyy-beginning-of-code)))

(defun lispyy-forward-to-end-of-code ()   ;;  test
  "TODO: documentation"
  (interactive)
  (if (lispyy-before-end-of-code-p)
      (lispyy-end-of-code)))

;;;;
;;;;  Beginning/end of lines
;;;;

(defun lispyy-backward-to-beginning-of-line ()   ;;  test
  "TODO: documentation"
  (interactive)
  (when (lispyy-after-beginning-of-line-p)
    (beginning-of-line)
    t))

(defun lispyy-forward-to-end-of-line ()   ;;  test
  "TODO: documentation"
  (interactive)
  (when (lispyy-before-end-of-line-p)
    (end-of-line)
    t))

(defun lispyy-beginning-of-line ()   ;  test
  "TODO: documentation"
  (interactive)   ;  test 2
  (or (lispyy-backward-to-beginning-of-comment-text)
      (lispyy-backward-to-beginning-of-comment)
      (lispyy-backward-to-beginning-of-code)
      (lispyy-backward-to-beginning-of-line)
      (lispyy-beginning-of-code)))

;; (define-key lispy-mode-map (kbd "s-x") 'lispyy-forward-to-end-of-code)

(defun lispyy-end-of-line ()   ;;  test
  "TODO: documentation"
  (interactive)
  (or (lispyy-forward-to-end-of-code)
      (lispyy-forward-to-beginning-of-comment)
      (lispyy-forward-to-end-of-line)
      (lispyy-end-of-code)
      (lispyy-beginning-of-comment)))

;; (define-key lispy-mode-map (kbd "s-x") 'lispyy-before-end-of-code-p)
;; (define-key lispy-mode-map (kbd "s-y") 'lispyy-before-beginning-of-comment-p)
;; (define-key lispy-mode-map (kbd "s-z") 'lispyy-before-end-of-line-p)
;; (define-key lispy-mode-map (kbd "s-c") 'lispyy-end-of-code)

(defun lispyy-beginning-of-defun ()
  "TODO: documentation"
  (interactive)
  (if (save-excursion
        (while (and (not (looking-at-p "(defun *\\|(defun$"))
                    (lispy-left 1)))
        (looking-at-p "(defun *\\|(defun$"))
      (while (and (not (looking-at-p "(defun *\\|(defun$"))
                  (lispy-left 1)))))

;;;;
;;;;  lispyy-forward/-backward commands
;;;;

;; (define-key lispy-mode-map (kbd "{") 'lispy-braces)
'(define-key lispy-mode-map (kbd "[") 'lispyy-backward)
'(define-key lispy-mode-map (kbd "]") 'lispyy-forward)

(defvar-local lispyy-forward-backward-repeat-count 0)

(defun lispyy-forward (&optional arg)
  "TODO: documentation"
  (interactive "p")
  (setq arg (or arg 1))
  (lispy-forward arg)
  (if (eq (msg-- last-command) 'lispyy-backward)
      (setq lispyy-forward-backward-repeat-count (1+ lispyy-forward-backward-repeat-count))
    (setq lispyy-forward-backward-repeat-count 1))
  (if (> lispyy-forward-backward-repeat-count 2)
      (lispyy-center-sexp)))

(defun lispyy-backward (&optional arg)
  "TODO: documentation"
  (interactive "p")
  (setq arg (or arg 1))
  (lispy-backward arg)
  (if (eq last-command 'lispyy-forward)
      (setq lispyy-forward-backward-repeat-count (1+ lispyy-forward-backward-repeat-count))
    (setq lispyy-forward-backward-repeat-count 1))
  (if (> lispyy-forward-backward-repeat-count 2)
      (lispyy-center-sexp)))

;; (defun lispy-forward1 (&optional arg)
;;   "TODO: documentation"
;;   (interactive "p")
;;   (lispy-forward arg)
;;   (set-transient-map
;;    (let ((map (make-sparse-keymap)))
;;      (define-key map (kbd "[") 'lispy-backward2)
;;      map)))

;;;;
;;;;  lispyy-left/-right/-up/-down commands
;;;;

(defun lispyy-left ()
  "TODO: documentation"
  (interactive)
  (cond ((lispy-right-p)
         (let ((not-lispyy-flow-p      (not (lispyy-flow-p)))
               (not-lispyy-different-p (not (lispyy-different-p))))
           (cond ((and not-lispyy-flow-p not-lispyy-different-p) nil)
                 (not-lispyy-flow-p      (lispy-different))
                 (not-lispyy-different-p (lispy-flow 1))
                 (t
                  (goto-char (max (position-of 'lispy-flow)
                                  (position-of 'lispy-different)))))))
        ((lispy-left-p) (special-lispy-left))
        (t (special-lispy-left))))

(defun lispyy-right ()
  "TODO: documentation"
  (interactive)
  (cond ((lispy-right-p) (special-lispy-right))
        ((lispy-left-p)
         (let ((not-lispyy-flow-p      (not (lispyy-flow-p)))
               (not-lispyy-different-p (not (lispyy-different-p))))
           (cond ((and not-lispyy-flow-p not-lispyy-different-p) nil)
                 (not-lispyy-flow-p      (lispy-different))
                 (not-lispyy-different-p (lispy-flow 1))
                 (t
                  (goto-char (min (position-of 'lispy-flow)
                                  (position-of 'lispy-different)))))))
        (t (special-lispy-right))))

(defun lispyy-up ()
  "TODO: documentation"
  (interactive)
  (if (looking-at-p hl-header-block-re)
      (lispyy-header-prev 1)
    (cond ((lispy-right-p) (lispy-up 1))
          ((lispy-left-p)
           (let ((pt (point)))
             (lispy-up 1)
             (when (= (line-number-at-pos)
                      (line-number-at-pos pt))
               (lispy-left 1))))
          (t (special-lispy-up)))))

(defun lispyy-down ()
  "TODO: documentation"
  (interactive)
  (if (looking-at-p hl-header-block-re)
      (lispyy-header-next 1)
    (cond ((lispy-right-p) (special-lispy-down))
          ((lispy-left-p)
           (let ((pt (point)))
             (lispy-down 1)
             (when (= (line-number-at-pos)
                      (line-number-at-pos pt))
               (when (lispy-flow 1)
                 (while (and (= (line-number-at-pos)
                                (line-number-at-pos pt))
                             (lispy-down 1)))))))
          (t (special-lispy-down)))))

;;;;
;;;;  lispyy-jump-closing-parens
;;;;

(defun lispyy-jump-closing-paren ()
  "TODO: documentation"
  (interactive)
  (when (and (not (lispyy-in-string-p))
             (lispyy-code-to-left-p))
    (while (looking-back-on-line " ")
      (delete-char -1))
    (if (looking-at-p ")")
        (right-char))))

(defun lispyy-jump-closing-parens ()
  "TODO: documentation"
  (interactive)
  ;; (lispyy-do-while 'right-char (lambda () (looking-at-p " *)"))
  ;;                  'eolp 'lispyy-nop)
  (while (looking-at-p " *)")
    (right-char)))

;;;
;;;  Editing
;;;

;;;;
;;;;  Killing
;;;;

(defun lispyy-forward-delimited-sexp ()
  "TODO: documentation"
  (interactive)
  (cond ((lispyy-in-string-p)
         (msg-- "in string")
         (if (lispyy-in-string-at-end-p)
             nil
           (right-char)
           t))
        (t
         (msg-- "not in string")
         (ignore-errors
           (forward-sexp)
           t))))

(defun lispyy-kill-line-of-code-end-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let ((limit-pos (lispyy-line-of-code-end-position)))
      (while (and (< (point) limit-pos)
                  (lispyy-forward-delimited-sexp)))
      (msg-- (point)))))

(defun lispyy-kill-rest-of-line (&optional arg)
  "TODO: documentation"
  (interactive "p")
  (kill-region (point) (line-end-position)))

(defun lispyy-kill-line (&optional arg)
  "TODO: documentation"   ;;  test
  (interactive "p")
  (let ((pt                    (point))
        (kill-from-column      (current-column))
        (this-line-indentation (lispyy-this-line-indentation)))   ;;  test
    (progn
      (cond ((lispyy-in-string-p)                (progn (msg- "lispyy-kill-line: lispyy-in-string-p")
                                                        (lispyy-kill-line-of-string)))
            ((lispyy-looking-at-or-in-comment-p) (progn (msg- "lispyy-kill-line: lispyy-looking-at-or-in-comment-p")
                                                        (lispyy-kill-line-of-comment)))
            ((lispyy-line-empty-p)               (progn (msg- "lispyy-kill-line: lispyy-line-empty-p")
                                                        (kill-line 1)
                                                        (when (lispyy-next-line-has-code-p)
                                                          (if (lispyy-right-p) (insert " ")))
                                                        (if (lispyy-code-to-right-p)
                                                            (lispyy-dedent-to-point))))
            ((lispyy-after-code-p)               (progn (msg- "lispyy-kill-line: lispyy-after-code-p")
                                                        (when (lispyy-next-line-has-code-p)
                                                          (if (lispyy-right-p) (insert " "))
                                                          (kill-line 1))
                                                        (if (lispyy-code-to-right-p)
                                                            (lispyy-dedent-to-point))))
            ((looking-at-p ")")                  (progn (msg- "lispyy-kill-line: looking-at-p \")\"")
                                                        (lispyy-jump-closing-paren)))
            ((lispyy-code-to-right-p)            (progn (msg- "lispyy-kill-line: lispyy-code-to-right-p")
                                                        (lispyy-kill-line-of-code kill-from-column this-line-indentation)))
            (t                                   (error "lispyy-kill-line: How did we get here?"))))))

"First line
Second line"

(defun lispyy-kill-line-of-string ()
  (save-excursion
    ;; (if (< (current-column) (lispyy-this-line-indentation))
    ;;     (back-to-indentation))
    ;; (kill-region (point) (lispyy-kill-line-of-code-end-position))
    ;; (lispyy-kill-sexp)
    (cond ((lispyy-in-string-at-end-p)         (right-char))
          ((eolp)                              (kill-line))
          ((< (lispyy-end-of-line-position)
              (lispyy-end-of-string-position)) (lispyy-kill-rest-of-line))
          (t
           (lispyy-kill-sexp)))))

(defun lispyy-kill-line-of-code (kill-from-column this-line-indentation)
  (save-excursion
    ;; (if (< (current-column) (lispyy-this-line-indentation))
    ;;     (back-to-indentation))
    (kill-region (point) (lispyy-kill-line-of-code-end-position))

    (let ((next-line-indentation (lispyy-next-line-indentation)))
      (cond ((and (<= kill-from-column this-line-indentation)
                  (= this-line-indentation next-line-indentation)) (progn (msg- "lispyy-kill-line-of-code: identation condition")
                                                                          (append-next-kill)
                                                                          (kill-line)
                                                                          (delete-char kill-from-column)
                                                                          ;; (delete-char this-line-indentation)
                                                                          ;; (lispyy-dedent-to-point)
                                                                          ))
            ((lispyy-line-empty-p)                                 (progn (msg- "lispyy-kill-line-of-code: lispyy-line-empty-p")
                                                                          (append-next-kill)
                                                                          (kill-line)
                                                                          (lispyy-dedent-to-point)
                                                                          ;; (lispy-dedent-adjust-parens)
                                                                          ))
            ((not (lispyy-after-code-p))                           (progn (msg- "lispyy-kill-line-of-code: not lispyy-after-code-p")
                                                                          (lispyy-dedent-to-point)))
            ((lispyy-after-code-p)                                 (progn (msg- "lispyy-kill-line-of-code: lispyy-after-code-p")
                                                                          (when (= kill-from-column next-line-indentation)
                                                                            (append-next-kill)
                                                                            (kill-line)
                                                                            (lispyy-dedent-to-point))))))))

;; (defun lispy-different ()
;;   "Switch to the different side of current sexp."
;;   (interactive)
;;   (cond ((and (region-active-p)
;;               (not (= (region-beginning) (region-end))))
;;          (exchange-point-and-mark))
;;         ((lispy-right-p)
;;          (backward-list))
;;         ((lispy-left-p)
;;          (forward-list))
;;         (t
;;          (user-error "Unexpected"))))

(defun lispyy-different ()
  "TODO: documentation"
  (interactive)
  (cond ((or (lispy-left-p)
             (lispy-right-p)) (lispy-different))
        ((lispyy-left-q)      (let ((bounds (lispy--bounds-string)))
                                (goto-char (cdr bounds))))
        ((lispyy-right-q)     (let ((bounds (save-excursion
                                              (left-char)
                                              (lispy--bounds-string))))
                                (goto-char (car bounds))))
        (t                    (error "lispyy-different: How did we get here?"))))

(defun lispyy-kill-sexp-end-position ()
  "TODO: documentation"
  (interactive)
  (cond ((lispyy-left-q) (save-excursion
                           (lispyy-different)
                           (msg-- (point))))
        (t               (save-excursion
                           (while (lispyy-forward-delimited-sexp))
                           (msg-- (point))))))

(defun lispyy-kill-sexp (&optional arg)
  "TODO: documentation"
  (interactive "p")
  (let ((kill-from-column      (current-column))
        (this-line-indentation (lispyy-this-line-indentation)))
    (save-excursion
      (kill-region (point) (lispyy-kill-sexp-end-position))
      (let ((next-line-indentation (lispyy-next-line-indentation)))
        (cond ((and (<= kill-from-column this-line-indentation)
                    (= this-line-indentation next-line-indentation)) (progn (msg- "lispyy-kill-sexp: indentation condition")
                                                                            (append-next-kill)
                                                                            (kill-line)
                                                                            (delete-char kill-from-column)
                                                                            ;; (delete-char this-line-indentation)
                                                                            ;; (lispyy-dedent-to-point)
                                                                            ))
              ((not (lispyy-after-code-p))                           (progn (msg- "lispyy-kill-sexp: not lispyy-after-code-p")
                                                                            (lispyy-dedent-to-point)))
              ((lispyy-after-code-p)                                 (progn (msg- "lispyy-kill-sexp: lispyy-after-code-p")
                                                                            (when (= kill-from-column next-line-indentation)
                                                                              (append-next-kill)
                                                                              (kill-line)
                                                                              (lispyy-dedent-to-point)))))))))

;; (defun lispyy-kill-line-of-code-end-position-0 ()   ;;  comment
;;   "TODO: documentation"
;;   (interactive)
;;   (let ((pos (point))
;;         end-of-kill-pos)
;;     (lispyy-end-of-code)
;;     (let ((limit-pos (point)))
;;       (goto-char pos)
;;       (let ((focus-pos pos))
;;         (while (and (<= focus-pos limit-pos)
;;                     (not end-of-kill-pos))
;;           (when (and (lispy-right-p)
;;                      (> focus-pos pos))
;;             (if (< (lispyy-position-other-end) pos)
;;                 (setq end-of-kill-pos (1- focus-pos))))
;;           (when (lispy-left-p)
;;             (if (> (lispyy-position-other-end) limit-pos)
;;                 (setq end-of-kill-pos (lispyy-position-other-end))))
;;           (lispyy-next)
;;           (setq focus-pos (point))))
;;       (goto-char pos)
;;       (setq end-of-kill-pos (or end-of-kill-pos (line-end-position)))
;;       (msg-- end-of-kill-pos))))

'((a
   b
   c))

;;;;
;;;;  Moving parentheses
;;;;

(defun lispyy-move-paren-left (&optional arg)
  (interactive "p")
  (cond ((lispy-right-p) (lispy-barf arg))
        ((lispy-left-p)  (lispy-slurp arg))
        (t               (error "lispyy-move-paren-left: How did we get here?"))))

(defun lispyy-move-paren-right (&optional arg)
  (interactive "p")
  (cond ((lispy-right-p) (lispy-slurp arg))
        ((lispy-left-p)  (lispy-barf arg))
        (t               (error "lispyy-move-paren-right: How did we get here?"))))

;;;;
;;;;  Newlines
;;;;

(defun lispyy-newline ()
  "TODO: documentation"
  (interactive)
  (if (and (lispyy-line-has-trailing-comment-p)
           (lispyy-at-trailing-closing-parens-p)) ;  comment
      (progn (lispyy-save-trailing-comment)
             (call-interactively 'newline)
             (lispyy-restore-trailing-comment))
    (lispy-newline-and-indent)))        ;  comment

(defun lispyy-insert-newline-at-pos (pos)
  "TODO: documentation"
  (interactive)
  (let ((pt (point)))
    (goto-char pos)
    (call-interactively 'lispy-newline-and-indent)
    (goto-char pt)))

;;;;
;;;;  Indentation
;;;;

(defun lispyy-indent-to-column (col)
  "TODO: documentation"
  (interactive "p")
  (lispyy-do-until (lambda () (interactive) (insert " "))
                   (lambda () (= (current-column) col))))

(defun lispyy-dedent-to-point ()
  "TODO: documentation"
  (interactive)
  (delete-region (point) (progn
                           (skip-chars-forward " \t")
                           (point))))

(defun lispyy-this-line-indentation ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun lispyy-next-line-indentation ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (current-column)))

;;;;
;;;;  Cuddled parens
;;;;

(defun lispyy-cuddle-parens ()
  "TODO: documentation"
  (interactive)
  (cond ((lispy-right-p)
         (when (looking-at-p "[ \t\n]+)")
           (lispy-forward 1)
           (special-lispy-flow)))
        ((lispy-left-p)
         (when (looking-back "([ \t\n]+" (point-min))
           (delete-region (1+ (match-beginning 0))
                          (match-end 0))))))

(defun lispyy-insert-newline-after-cuddled-sublist ()
  "TODO: documentation"
  (interactive)
  (let ((pt (point)))
    (lispy-forward 1)
    (if (looking-at-p " *)")
        (call-interactively 'lispy-newline-and-indent))
    (goto-char pt)))

;;;;
;;;;  Trailing whitespace
;;;;

(defun lispyy-delete-trailing-whitespace ()
  "TODO: documentation"
  (interactive)
  (end-of-line)
  (while (looking-back-on-line " ")
    (delete-char -1)))

;;
;; (define-key lispy-mode-map (kbd "M-1") 'lispyy-count-lines)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(quote ((one two) (three four) () ()))

(defun lispyy-position-other-end ()
  "TODO: documentation"
  (interactive)
  (cond ((lispyy-lispy-p)
         (save-excursion
           (lispy-different)
           (msg-- (point))))
        ((lispyy-left-q) (msg-- (cdr (lispy--bounds-string))))
        ((lispyy-right-q) (save-excursion
                            (left-char)
                            (msg-- (car (lispy--bounds-string)))))))

;; (defun lispyy-next-lispy ()
;;   "TODO: documentation"
;;   (interactive)
;;   (lispyy-do-until 'right-char 'lispyy-lispy-p 'eobp 'lispyy-nop))

(quote
 ())

;;;
;;;  Commenting
;;;

;;;;
;;;;  Lines
;;;;

(defun lispyy-comment-line (&optional column prefix)
  "TODO: documentation"
  (interactive)
  (setq column (or column (current-indentation)))
  (setq prefix (or prefix ";; "))
  (move-to-column column)
  (insert prefix))

(defun lispyy-comment-lines (column semicolons lines)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let ((line 1))
      (while (<= line lines)
        (move-to-column column t)
        (if (eolp)
            (insert semicolons)
          (insert (concat semicolons " ")))
        (forward-line)
        (setq line (1+ line))))))

;;;;
;;;;  Lists
;;;;

;; (defun lispyy-comment-list ()
;;   "TODO: documentation"
;;   (interactive)
;;   (save-excursion
;;     (let (comment/line/column)
;;       (if (lispy-bolp)
;;           (back-to-indentation))
;;       (if (lispyy-comment-after-list-p)
;;           (setq comment/line/column (lispyy-save-comment-after-list)))
;;       (lispyy-insert-newline-after-cuddled-sublist)
;;       (when comment/line/column
;;         (lispyy-restore-comment-after-list comment/line/column)
;;         (setq comment/line/column nil))
;;       (let ((last-line-number (lispyy-last-line-number-of-list)))
;;         (lispyy-comment-list-helper last-line-number))))) ;  comment

;; (defun lispyy-comment-list-helper (last-line)
;;   "TODO: documentation"
;;   (interactive)
;;   (let ((pt (point))
;;         (col (current-column)))
;;     (while (<= (line-number-at-pos) last-line)
;;       (move-to-column col t)
;;       (if (eolp)
;;           (insert ";;")
;;         (insert ";; "))
;;       (forward-line))
;;     (back-to-indentation)))

;;;;
;;;;  Code
;;;;

(defun lispyy-comment-sexp ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let* ((end-of-kill-pos  (lispyy-kill-line-of-code-end-position))
           (last-line-number (line-number-at-pos end-of-kill-pos))
           comment/line/column)
      (if (lispy-bolp)
          (back-to-indentation))
      ;; (if (lispyy-comment-after-pos-p end-of-kill-pos)
      ;;     (setq comment/line/column (lispyy-save-comment-after-pos end-of-kill-pos)))
      ;; (if (lispyy-before-beginning-of-comment-p end-of-kill-pos)
      ;;     (setq comment/line/column (lispyy-save-comment-after-pos end-of-kill-pos)))
      ;; (if (lispyy-on-line-with-comment-p end-of-kill-pos)
      ;;     (setq comment/line/column (lispyy-save-comment-after-pos end-of-kill-pos)))
      (if (lispyy-line-has-comment-p end-of-kill-pos)
          (setq comment/line/column (lispyy-save-comment-after-pos end-of-kill-pos)))
      (if (not (lispyy-eocp end-of-kill-pos))
          (lispyy-insert-newline-at-pos end-of-kill-pos))
      (when comment/line/column
        (lispyy-restore-comment comment/line/column)
        (setq comment/line/column nil))
      (lispyy-comment-sexp-helper last-line-number))))

(defun lispyy-comment-sexp-helper (last-line)
  "TODO: documentation"
  (interactive)
  (let ((col (current-column)))
    (while (<= (line-number-at-pos) last-line)
      (move-to-column col t)
      (if (eolp)
          (insert ";;")
        (insert ";; "))
      (forward-line))
    (back-to-indentation)))

;; (defun lispyy-comment-sexp ()
;;   "TODO: documentation"
;;   (interactive)
;;   (save-excursion
;;     (let* ((end-of-kill-pos (lispyy-kill-line-of-code-end-position))
;;            (last-line-number (line-number-at-pos end-of-kill-pos))
;;            comment/line/column)
;;       ;; (if (lispy-bolp)
;;       ;;     (back-to-indentation))
;;       (if (lispyy-comment-after-pos-p end-of-kill-pos)
;;           (setq comment/line/column (lispyy-save-comment-after-pos end-of-kill-pos)))
;;       (if (not (lispyy-eocp end-of-kill-pos))
;;           (lispyy-insert-newline-at-pos end-of-kill-pos))
;;       (when comment/line/column
;;         (lispyy-restore-comment comment/line/column)
;;         (setq comment/line/column nil))
;;       (lispyy-comment-sexp-helper last-line-number))))

;;;;
;;;;  Uncommenting
;;;;

(defun lispyy-uncomment-line (&optional column prefix)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (setq column (or column (current-indentation)))
    ;; (setq prefix (or prefix ";+ +"))
    (setq prefix (or prefix ";+ "))
    (move-to-column column)
    (if (looking-at prefix)
        (kill-region
         (match-beginning 0)
         (match-end 0)))))

(defun lispyy-uncomment-block ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let ((column (current-indentation))
          (semicolons-regexp ";+"))
      (move-to-column column)
      (if (looking-at semicolons-regexp)
          (let* ((semicolons (match-string 0))
                 (prefix-regexp (concat semicolons " \\|" semicolons "$"))
                 (lines 1))
            (while (looking-at prefix-regexp)
              (setq lines (1+ lines))
              (delete-region (match-beginning 0)
                             (match-end 0))
              (forward-line)
              (move-to-column column))
            (cons column (cons semicolons lines)))))))

;; (defun lispyy-commented-sexp-lines ()
;;   "TODO: documentation"
;;   (interactive)
;;   (save-buffer-state
;;     (let ((pt (point))
;;           (column (current-indentation))
;;           (prefix ";+ \\|;+$"))
;;       (move-to-column column)
;;       (let* ((lines (cddr (lispyy-uncomment-block)))
;;              (end-of-kill-pos (lispyy-kill-line-of-code-end-position)))
;;         (goto-char end-of-kill-pos)
;;         (if (and (<= (line-number-at-pos end-of-kill-pos)
;;                      (+ (line-number-at-pos pt) lines -1))
;;                  (lispyy-eocp))
;;             (let ((commented-sexp-lines (count-lines pt end-of-kill-pos)))
;;               commented-sexp-lines))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg-- (lispyy-commented-sexp-lines))))

(defun lispyy-commented-sexp-lines ()
  "TODO: documentation"
  (interactive)
  (save-buffer-state
    (let ((pt (point)))
      (move-to-column (current-indentation))
      (let* ((lines (cddr (lispyy-uncomment-block)))
             (end-of-kill-pos (lispyy-kill-line-of-code-end-position)))
        (goto-char end-of-kill-pos)
        (if (and (<= (line-number-at-pos end-of-kill-pos)
                     (+ (line-number-at-pos pt) lines -1))
                 (lispyy-eocp))
            (let ((commented-sexp-lines (count-lines pt end-of-kill-pos)))
              commented-sexp-lines)
          nil)))))

;;  test
(defun lisppy-looking-at-commented-sexp-p ()
  "TODO: documentation"
  (interactive)
  (if (looking-at-p "[ \t]*;+[ \t](")
      (save-buffer-state
        (let ((pt (point)))
          (move-to-column (current-indentation))
          (let* ((lines (cddr (lispyy-uncomment-block)))
                 (end-of-kill-pos (lispyy-kill-line-of-code-end-position)))
            (goto-char end-of-kill-pos)
            (and (<= (line-number-at-pos end-of-kill-pos)
                     (+ (line-number-at-pos pt) lines -1))
                 (lispyy-eocp)))))))

;; (defun lispyy-uncomment-sexp ()
;;   "TODO: documentation"
;;   (interactive)
;;   (save-excursion
;;     (let ((pt (point))
;;           (column (current-indentation))
;;           (prefix ";+ \\|;+$"))
;;       (move-to-column column)
;;       (let* ((column/semicolons/lines (lispyy-uncomment-block))
;;              (column     (car column/semicolons/lines))
;;              (semicolons (cadr column/semicolons/lines))
;;              (lines      (cddr column/semicolons/lines))
;;              (end-of-kill-pos (lispyy-kill-line-of-code-end-position)))
;;         (goto-char end-of-kill-pos)
;;         (if (and (<= (line-number-at-pos end-of-kill-pos)
;;                      (+ (line-number-at-pos pt) lines -1))
;;                  (lispyy-eocp))
;;             (progn (forward-line)
;;                    (let ((uncommented-lines (count-lines pt end-of-kill-pos)))
;;                      (lispyy-comment-lines column semicolons (- lines uncommented-lines 1)))
;;                    (goto-char end-of-kill-pos)
;;                    (lispyy-save-trailing-comment)
;;                    (lispyy-cuddle-parens)
;;                    (lispyy-restore-trailing-comment))
;;           (goto-char pt)
;;           (lispyy-comment-lines column semicolons (1- lines)))))))

(defun lispyy-uncomment-sexp ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let ((pt (point))
          (column (current-indentation))
          (prefix ";+ \\|;+$"))
      (move-to-column column)
      (let* ((column/semicolons/lines (lispyy-uncomment-block))
             (column     (car column/semicolons/lines))
             (semicolons (cadr column/semicolons/lines))
             (lines      (cddr column/semicolons/lines))
             (end-of-kill-pos (lispyy-kill-line-of-code-end-position)))
        (goto-char end-of-kill-pos)
        (if (and (<= (line-number-at-pos end-of-kill-pos)
                     (+ (line-number-at-pos pt) lines -1))
                 (lispyy-eocp))
            (progn (forward-line)
                   (let ((uncommented-lines (count-lines pt end-of-kill-pos)))
                     (lispyy-comment-lines column semicolons (- lines uncommented-lines 1)))
                   (goto-char end-of-kill-pos)
                   (lispyy-save-trailing-comment)
                   (lispyy-cuddle-parens)
                   (lispyy-restore-trailing-comment))
          (goto-char pt)
          (lispyy-comment-lines column semicolons (1- lines)))))))   ;;  comment

;;;;
;;;;  DWIM
;;;;

(defun lispyy-comment-dwim ()
  "TODO: documentation"
  (interactive)
  (cond ((region-active-p)
         (lispy-comment))
        ((lispyy-on-whole-line-comment-p)
         ;; (lispyy-uncomment-line)
         (lispyy-uncomment-sexp))
        ;;  now this is an end-of-line comment ...
        ((lispyy-before-or-in-comment-p)
         (lispyy-end-of-code)
         (let ((kill-whole-line nil))
           (kill-line)))
        ;;  we're commenting now instead of uncommenting ...
        ((and (lispyy-bolp)
              (not (lispyy-line-has-code-p)))
         (insert ";;  "))
        ((lispyy-eolp)
         (insert "   ;;  "))
        (t (lispyy-comment-sexp))))

(defvar lispyy-saved-comment/line/column)

(defun lispyy-save-trailing-comment ()
  "TODO: documentation"
  (interactive)
  (setq lispyy-saved-comment/line/column nil)
  (let ((pt (point)))
    (and (lispyy-beginning-of-comment)
         (let ((line    (line-number-at-pos))
               (column  (current-column))
               (comment (delete-and-extract-region (point) (line-end-position))))
           (lispyy-do-while 'delete-backward-char (lambda () (looking-back-on-line " ")))
           (setq lispyy-saved-comment/line/column (cons comment (cons line column)))))
    (goto-char pt)))

(defun lispyy-save-comment-after-list ()
  "TODO: documentation"
  (interactive)
  (let ((pt (point)))
    (lispy-forward 1)
    (lispyy-do-while 'right-char '(lambda () (looking-at-p " *)"))
                     'eolp 'lispyy-nop)
    (cond ((lispyy-before-comment-p)
           (end-of-line)
           ;; (lispyy-do-while 'left-char 'lispyy-in-comment-p)
           (lispyy-do-until 'left-char 'lispyy-before-comment-p)
           (let ((line    (line-number-at-pos))
                 (column  (current-column))
                 (comment (delete-and-extract-region (point) (line-end-position)))
                 comment/line/column)
             (lispyy-do-while 'delete-backward-char (lambda () (looking-back-on-line " ")))
             (goto-char pt)
             (setq comment/line/column (cons comment (cons line column)))
             (message- "here: %S" comment/line/column)
             (message- "here2: %S" (cons 1 (cons 2 3)))
             comment/line/column)
           )
          (t (goto-char pt)
             nil))))

(defun lispyy-save-comment-after-pos (pos)
  "TODO: documentation"
  (interactive)
  (save-excursion
    (goto-char pos)
    ;; (lispyy-jump-closing-parens)
    (lispyy-end-of-code)
    (cond ((lispyy-before-comment-p)
           (lispyy-beginning-of-comment)
           (let ((line    (line-number-at-pos))
                 (column  (current-column))
                 (comment (delete-and-extract-region (point) (line-end-position)))
                 comment/line/column)
             (lispyy-delete-trailing-whitespace)
             (setq comment/line/column (cons comment (cons line column)))
             comment/line/column))
          (t nil))))

(defun lispyy-restore-comment (&optional comment/line/column)
  "TODO: documentation"
  (interactive "p")
  (setq comment/line/column (or comment/line/column lispyy-saved-comment/line/column))
  (message- "here6: %S" comment/line/column)
  (let ((pt (point))
        (comment (car  comment/line/column))
        (line    (cadr comment/line/column))
        (column  (cddr comment/line/column)))
    ;; (goto-line line)
    ;; (forward-line (- line (current-line)))
    (goto-char (point-min))
    (forward-line (1- line))            ;  comment
    (lispyy-end-of-code)
    (if t
        (progn (insert comment)
               ;; (lispyy-do-until 'left-char 'lispyy-before-comment-p)
               (lispyy-beginning-of-comment)
               (lispyy-do-until (lambda () (interactive) (insert " "))
                                (lambda () (>= (current-column) column))) ;  comment
               (setq lispyy-saved-comment/line/column nil))
      (lispyy-restore-trailing-comment comment/line/column))
    (goto-char pt)))                    ; test

(defun lispyy-restore-trailing-comment (&optional comment/line/column)
  "TODO: documentation"
  (interactive)
  (setq comment/line/column (or comment/line/column lispyy-saved-comment/line/column))
  (let ((pt (make-marker)))
    (set-marker pt (point))
    (when comment/line/column
      (let ((comment (car  comment/line/column))
            (line    (cadr comment/line/column))
            (column  (cddr comment/line/column))) ;  comment
        (forward-line (- line (line-number-at-pos)))
        (lispyy-end-of-code)
        (insert (concat "" comment))
        (lispyy-beginning-of-comment)
        (lispyy-do-until (lambda () (interactive) (insert " "))
                         (lambda () (>= (current-column) column)))   ;  comment
        ;; (lispyy-do-while-still (lambda () (interactive) (insert " "))
        ;;                        (lambda () (< (current-column) column)))   ;  comment
        (setq lispyy-saved-comment/line/column nil)))
    (goto-char pt)))

(defun lispyy-restore-comment-after-list (&optional comment/line/column)
  "TODO: documentation"
  (interactive "p")
  (setq comment/line/column (or comment/line/column lispyy-saved-comment/line/column))
  (message- "here6: %S" comment/line/column)
  (let ((pt (point))
        (comment (car  comment/line/column))
        (line    (cadr comment/line/column))
        (column  (cddr comment/line/column)))
    (lispy-forward 1)
    (lispyy-do-while 'right-char '(lambda () (looking-at-p " *)"))
                     'eolp 'lispyy-nop)
    (if t
        (progn (insert comment)
               ;; (lispyy-do-until 'left-char 'lispyy-before-comment-p)
               (lispyy-beginning-of-comment)
               (lispyy-do-until (lambda () (interactive) (insert " "))
                                (lambda () (>= (current-column) column))) ;  comment
               (setq lispyy-saved-comment/line/column nil))
      (lispyy-restore-trailing-comment comment/line/column))
    (goto-char pt)                                        ;  comment
    ))

(defun lispyy-do-ignore-trailing-comment (cmd)
  "TODO: documentation"
  (interactive)
  (lispyy-save-trailing-comment)
  (let ((pt (point)))
    (apply cmd)
    (goto-char pt))
  (lispyy-restore-trailing-comment))

'(global-set-key (kbd "M-t") 'lispyy-comment-dwim)
'(global-set-key (kbd "M-1") 'lispyy-beginning-of-list-p)

(quote
 ((a) (b
       c)
  d))

'(global-set-key (kbd "M-1") 'lispyy-before-comment-p)
'(global-set-key (kbd "M-1") 'lispyy-in-comment-p)
'(global-set-key (kbd "M-2") 'lispyy-beginning-of-comment)
'(global-set-key (kbd "M-2") 'lispyy-beginning-of-comment)
'(global-set-key (kbd "M-3") 'lispyy-comment-after-list-p)
'(global-set-key (kbd "M-4") 'lispyy-save-comment-after-list)
'(global-set-key (kbd "M-5") 'lispyy-insert-newline-after-cuddled-sublist)
'(global-set-key (kbd "M-6") 'lispyy-comment-list)

(quote
 (zero
  half (two-thirds)
  ((((one
      two)            ;  comment

     three)
    four))))          ;  comment

;;;;
;;;;  Killing
;;;;

(defun lispyy-kill-comment ()
  "TODO: documentation"
  (interactive)
  (when (lispyy-looking-at-comment-p)
    (cond ((lispyy-after-code-p)                (progn (msg- "lispyy-kill-comment: lispyy-after-code-p")
                                                       (let ((kill-whole-line nil))
                                                         (kill-line))))
          ((lisppy-looking-at-commented-sexp-p) (progn (msg- "lispyy-kill-comment: lisppy-looking-at-commented-sexp-p")
                                                       (lispyy-kill-commented-sexp)))
          ;;  not after code and not commented code
          (t                                    (progn (msg- "lispyy-kill-comment: t")
                                                       (let ((kill-whole-line nil))
                                                         (kill-line 1))))))
  ;; (cond ((lispyy-after-code-before-comment-p)
  ;;        (if (not (eolp))
  ;;            (let ((kill-whole-line nil))
  ;;              (kill-line))))
  ;;       ((looking-at-p " *;; +(")
  ;;        (msg-- "lispyy-kill-comment -> lispyy-kill-commented-sexp")
  ;;        (lispyy-kill-commented-sexp))
  ;;       ((lisppy-looking-at-commented-sexp-p)
  ;;        (lispyy-kill-commented-sexp))
  ;;       (t
  ;;        (let ((kill-whole-line nil))
  ;;          (kill-line))))
  )

(defun lispyy-kill-line-of-comment ()
  "TODO: documentation"
  (interactive)
  (cond ((eolp) nil)
        ((lispyy-within-comment-p)     (lispyy-kill-rest-of-line))
        ((lispyy-looking-at-comment-p) (cond ((lispyy-after-code-p) (progn (msg- "lispyy-kill-comment: lispyy-after-code-p")
                                                                           (let ((kill-whole-line nil))
                                                                             (kill-line))))
                                             ((lisppy-looking-at-commented-sexp-p) (progn (msg- "lispyy-kill-comment: lisppy-looking-at-commented-sexp-p")
                                                                                          (lispyy-kill-commented-sexp)))
                                             ;;  not after code and not commented code
                                             (t (progn (msg- "lispyy-kill-comment: t")
                                                       (let ((kill-whole-line nil))
                                                         (kill-line 1))))))))

(defun lispyy-kill-commented-sexp ()
  "TODO: documentation"
  (interactive)
  (if (looking-at-p " *;; +(")
      (let ((commented-sexp-lines (lispyy-commented-sexp-lines)))
        (msg-- "lispyy-kill-commented-sexp -> kill-line")
        (kill-line (msg-- commented-sexp-lines)))))

;;;
;;;  Viewing
;;;

(defun lispyy-window-line ()
  "TODO: documentation"
  (interactive)
  (- (line-number-at-pos)
     (line-number-at-pos (window-start))))

;; (defun lispyy-window-line ()
;;   "TODO: documentation"
;;   (interactive)
;;   (let ((window-line0 (count-lines (window-start) (point)))
;;         (window-line1 (- (line-number-at-pos) (line-number-at-pos (window-start)))))
;;     (if (/= window-line0 window-line1)
;;         (error "window-line calculation methods not equal."))
;;     (- (line-number-at-pos) (line-number-at-pos (window-start)))
;;     ;; window-line1
;;     ))

(defun lispyy-sexp-height ()
  "TODO: documentation"
  (interactive)
  (let* ((sexp-bounds (lispyy-bounds-list))
         (sexp-height (count-lines (car sexp-bounds) (cdr sexp-bounds))))
    (msg-- sexp-height)))

;; (defun lispyy-center-sexp-line ()
;;   "TODO: documentation"
;;   (interactive)
;;   (let ((window-height (window-body-height))
;;         (sexp-height   (lispyy-sexp-height)))
;;     (cond ((lispy-right-p)
;;            (- window-height (/ (- window-height sexp-height) 2) 1))
;;           ((lispy-left-p)
;;            (- (/ window-height 2) (/ sexp-height 2))))))

(defun lispyy-next-view-position ()
  "TODO: documentation"
  (interactive)
  (let* ((window-height (window-body-height))
         (window-line   (lispyy-window-line))
         (view-lines    (sort (list (lispyy-top-view-position)
                                    (lispyy-center-view-position)
                                    (lispyy-bottom-view-position)
                                    ;; (lispyy-original-view-position)
                                    ) '>))
         (view-lines-above (cl-remove-if-not (lambda (n) (and (>= n 0)
                                                              (< n window-height)
                                                              (< n window-line))) view-lines)))
    (if view-lines-above
        (car view-lines-above)
      (car view-lines))))

(defun lispyy-view-next-position ()
  "TODO: documentation"
  (interactive)
  (recenter (lispyy-next-view-position)))

(defun lispyy-view ()
  "TODO: documentation"
  (interactive)
  (lispyy-update-original-view-position)
  (let ((next-view-position (lispyy-next-view-position)))
    (recenter next-view-position)
    (if (= next-view-position
           (lispyy-original-view-position))
        (error "Original view position."))))

'(remove-duplicates (sort '(4 8 21 17 33 7 21 7) '<)
                    :test '=
                    :from-end t)

'(remove-if-not #'evenp '(1 2 3 4 5))

;;;;
;;;;  View position predicates
;;;;

(defun lispyy-line-centered-p ()
  "TODO: documentation"
  (interactive)
  (let ((window-line (count-lines (window-start) (point)))
        (window-height (window-body-height)))
    (msg-- (= window-line
             (1+ (/ window-height 2))))))

'(global-set-key (kbd "M-1") 'lispyy-sexp-at-bottom-p)
'(global-set-key (kbd "M-2") 'lispyy-sexp-at-bottom)

(defun lispyy-sexp-at-top-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if (not (lispy-left-p)) (lispy-backward 1))
    (let (;; (window-line (count-lines (window-start) (point)))
          ;; (window-line (- (line-number-at-pos) (line-number-at-pos (window-start))))
          (window-line (lispyy-window-line)))
      (= window-line 0))))

(defun lispyy-sexp-centered-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let ((window-height (window-body-height))
          (sexp-height   (lispyy-sexp-height))
          (window-line   (lispyy-window-line)))
      (cond ((lispy-right-p)
             (= window-line
                ;; (msg-- (- window-height (/ (- window-height sexp-height) 2) 1))
                (msg-- (lispyy-center-view-position))))
            ((lispy-left-p)
             (= window-line
                ;; (msg-- (- (/ window-height 2) (/ sexp-height 2)))
                (msg-- (lispyy-center-view-position))))))))

(defun lispyy-sexp-at-bottom-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (if (not (lispy-right-p)) (lispy-forward 1))
    (let ((window-line   (lispyy-window-line))
          (window-height (window-body-height)))
      (msg-- (= window-line
                (1- window-height))))))

(defun lispyy-view-top-position-p ()
  "TODO: documentation"
  (interactive)
  (= (lispyy-window-line)
     (lispyy-top-view-position)))

(defun lispyy-view-center-position-p ()
  "TODO: documentation"
  (interactive)
  (= (lispyy-window-line)
     (lispyy-center-view-position)))

(defun lispyy-view-bottom-position-p ()
  "TODO: documentation"
  (interactive)
  (= (lispyy-window-line)
     (lispyy-bottom-view-position)))

(defun lispyy-view-original-position-p ()
  "TODO: documentation"
  (interactive)
  (= (lispyy-window-line)
     (lispyy-original-view-position)))

'(global-set-key (kbd "M-1") 'lispyy-bounds-list)

'(lispy-define-key lispy-mode-map "v" 'lispyy-view)
'(lispy-define-key lispy-mode-map "V" 'lispyy-view-2)

;;;;
;;;;  Viewing commands
;;;;

(defun lispyy-view-position (&optional view-position)
  "TODO: documentation"
  (interactive)
  (if (not view-position)
      (let ((view-position (cons (point) (lispyy-window-line))))
        (msg-- view-position))
    (goto-char (car view-position))
    (recenter (cdr view-position))))

(defun lispyy-top-view-position ()
  "TODO: documentation"
  (interactive)
  (let ((sexp-height (lispyy-sexp-height)))
    (cond ((lispy-left-p)   0)
          ((lispy-right-p) (1- sexp-height))
          (t               (error "Not in lispy-special.")))))

(defun lispyy-center-view-position ()
  "TODO: documentation"
  (interactive)
  (let ((window-height (window-body-height))
        (sexp-height   (lispyy-sexp-height)))
    (cond ((lispy-left-p)  (if (<= sexp-height window-height)
                               (/ (- window-height sexp-height) 2)
                             0))
          ((lispy-right-p) (if (<= sexp-height window-height)
                               (- window-height (- (/ window-height 2) (/ sexp-height 2)) 1)
                             (1- window-height)))
          (t               (error "Not in lispy-special.")))))

(defun lispyy-bottom-view-position ()
  "TODO: documentation"
  (interactive)
  (let ((window-height (window-body-height))
        (sexp-height   (lispyy-sexp-height)))
    (cond ((lispy-left-p)  (- window-height
                              sexp-height))
          ((lispy-right-p) (1- window-height))
          (t               (error "Not in lispy-special.")))))

(defun lispyy-original-view-position ()
  "TODO: documentation"
  (interactive)
  (msg-- (cdr (get 'lispyy-original-view-position :view-position))))

(defun lispyy-update-original-view-position ()
  "TODO: documentation"
  (interactive)
  (let* ((window-line (lispyy-window-line))
         (sexp-bounds (lispyy-bounds-list))
         (view-position (cons (cons (point) sexp-bounds) window-line))
         (original-view-position (get 'lispyy-original-view-position :view-position)))
    (if (not (equal (car view-position) (car original-view-position)))
        (msg-- (put 'lispyy-original-view-position :view-position view-position)))))

(defun lispyy-view-top-sexp ()
  "TODO: documentation"
  (interactive)
  (if (not (lispyy-sexp-at-top-p))
      (save-excursion
        (if (not (lispy-left-p)) (lispy-backward 1))
        (recenter 0))))

(defun lispyy-center-sexp (&optional arg)
  "Center current sexp."
  (interactive "p")
  (save-excursion
    (let* ((window-height (window-body-height))
           (sexp-height (lispyy-sexp-height)))
      (cond ((<= sexp-height window-height) (cond ((lispy-left-p)  (when (not (lispyy-sexp-centered-p))
                                                                     (recenter (msg-- (lispyy-center-view-position)))))
                                                  ((lispy-right-p) (when (not (lispyy-sexp-centered-p))
                                                                     (recenter (msg-- (lispyy-center-view-position)))))))
            ((> sexp-height window-height)  (cond ((lispy-left-p)  (lispyy-view-top-sexp))
                                                  ((lispy-right-p) (lispyy-view-bottom-sexp))))
            (t                              (error "lispyy-center-sexp: How did we get here?"))))))

(defun lispyy-view-bottom-sexp ()
  "TODO: documentation"
  (interactive)
  (if (not (lispyy-sexp-at-bottom-p))
      (save-excursion
        (if (not (lispy-right-p)) (lispy-forward 1))
        (let ((window-height (window-body-height)))
          (recenter (- window-height 1))))))

(defun lispyy-view-top-position ()
  "TODO: documentation"
  (interactive)
  (recenter (lispyy-top-view-position)))

(defun lispyy-view-center-position ()
  "TODO: documentation"
  (interactive)
  (recenter (lispyy-center-view-position)))

(defun lispyy-view-bottom-position ()
  "TODO: documentation"
  (interactive)
  (recenter (lispyy-bottom-view-position)))

(defun lispyy-view-original-position ()
  "TODO: documentation"
  (interactive)
  (recenter (lispyy-original-view-position)))

;; (defun lispyy-view-2 ()
;;   "Center sexp, move to top, move to bottom, move to original position, repeat."
;;   (interactive "p")
;;   (let* ((window-height (window-body-height))
;;          (sexp-height (lispyy-sexp-height))
;;          (window-line (lispyy-window-line)))
;;     (lispyy-update-original-view-position)
;;     (cond ((<= sexp-height window-height)
;;            (cond ((and (not (lispyy-sexp-centered-p))
;;                        (not (lispyy-sexp-at-top-p))
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-center-sexp))
;;                  ((and (lispyy-sexp-centered-p)
;;                        (not (lispyy-sexp-at-top-p)))
;;                   (lispyy-view-top-sexp))
;;                  ((and (lispyy-sexp-at-top-p)
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-view-bottom-sexp))
;;                  ((and (lispyy-sexp-at-bottom-p)
;;                        (not (lispyy-view-original-position-p)))
;;                   (lispyy-view-original-position)
;;                   (error "Original view position."))))
;;           ((> sexp-height window-height)
;;            (cond ((and (not (lispyy-sexp-centered-p))
;;                        (not (lispyy-sexp-at-top-p))
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-center-sexp))
;;                  ((and (lispyy-sexp-centered-p)
;;                        (not (lispyy-sexp-at-top-p)))
;;                   (lispyy-view-top-sexp))
;;                  ((and (lispyy-sexp-at-top-p)
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-view-bottom-sexp))
;;                  ((and (lispyy-sexp-at-bottom-p)
;;                        (not (lispyy-view-original-position-p)))
;;                   (lispyy-view-original-position)
;;                   (error "Original view position.")))))))

;; (defun lispyy-view-2 ()
;;   "Center sexp, move to top, move to bottom, move to original position, repeat."
;;   (interactive "p")
;;   (let ((window-height (window-body-height))
;;         (sexp-height   (lispyy-sexp-height))
;;         (window-line   (lispyy-window-line)))
;;     (lispyy-update-original-view-position)
;;     (cond ((<= sexp-height window-height)
;;            (cond ((and (not (lispyy-sexp-centered-p))
;;                        (not (lispyy-sexp-at-top-p))
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-view-center-position))
;;                  ((and (lispyy-sexp-centered-p)
;;                        (not (lispyy-sexp-at-top-p)))
;;                   (lispyy-view-top-position))
;;                  ((and (lispyy-sexp-at-top-p)
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-view-bottom-position))))
;;           ((> sexp-height window-height)
;;            (cond ((and (not (lispyy-sexp-centered-p))
;;                        (not (lispyy-sexp-at-top-p))
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-center-sexp))
;;                  ((and (lispyy-sexp-centered-p)
;;                        (not (lispyy-sexp-at-top-p)))
;;                   (lispyy-view-top-sexp))
;;                  ((and (lispyy-sexp-at-top-p)
;;                        (not (lispyy-sexp-at-bottom-p)))
;;                   (lispyy-view-bottom-sexp)))))))

(defun lispyy-view-2 ()
  "Center sexp, move to top, move to bottom, move to original position, repeat."
  (interactive)
  (let ((window-height (window-body-height))
        (sexp-height   (lispyy-sexp-height))
        (window-line   (lispyy-window-line)))
    (lispyy-update-original-view-position)
    (if (<= sexp-height window-height)
        (cond ((and (not (lispyy-view-top-position-p))
                    (not (lispyy-view-center-position-p)))
               (lispyy-view-center-position))
              ((and (lispyy-view-center-position-p)
                    (not (lispyy-view-top-position-p)))
               (lispyy-view-top-position))
              ((and (lispyy-view-top-position-p)
                    (not (lispyy-view-bottom-position-p)))
               (lispyy-view-bottom-position)))
      (cond ((lispy-left-p)
             (if (lispyy-view-top-position-p)
                 (lispyy-view-original-position)
               (lispyy-view-top-position)))
            ((lispy-right-p)
             (if (lispyy-view-bottom-position-p)
                 (lispyy-view-original-position)
               (lispyy-view-bottom-position)))))))

;; (defun lispyy-view (&optional arg)
;;   "Recenter current sexp to first screen line, accounting for scroll-margin.
;; If already there, return it to previous position."
;;   (interactive "p")
;;   (lispy-from-left
;;    (let ((window-line (count-lines (window-start) (point))))
;;      (if (or (= window-line scroll-margin)
;;              (and (not (bolp)) (= window-line (1+ scroll-margin))))
;;          (recenter (or (get 'lispy-recenter :line) 0))
;;        (put 'lispy-recenter :line window-line)
;;        (recenter 0)))))

;; (lispy-define-key lispy-mode-map "j" 'special-lispyy-down)
;; (lispy-define-key lispy-mode-map "k" 'special-lispyy-up)

'(global-set-key (kbd "M-3") 'lispyy-left-p)
'(global-set-key (kbd "M-4") 'lispyy-right-p)
'(global-set-key (kbd "M-5") 'lispyy-center-sexp)

'(provide 'my-lispy)

;;;
;;;  Outlines
;;;

;;;;
;;;;  Header predicates
;;;;

(defun lispyy-in-header-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at-p hl-header-re)))

(defun lispyy-in-header-block-p ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at-p hl-header-block-re)))

;;;;
;;;;  Positions and bounds
;;;;

;;;;;
;;;;;  Header blocks
;;;;;

(defun lispyy-beginning-of-header-block-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at ";+"))
        (msg- nil)
      (let* ((semicolons    (match-string 0))
             (prefix-regexp (concat semicolons "$" "\\|" semicolons "[^;]")))
        (while (save-excursion (let ((pt (point)))
                                 (re-search-backward prefix-regexp nil t)
                                 (= (1+ (line-number-at-pos))
                                    (line-number-at-pos pt))))
          (re-search-backward prefix-regexp nil t)))
      (msg- (point)))))

(defun lispyy-end-of-header-block-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at ";+"))
        (msg- nil)
      (let* ((semicolons    (match-string 0))
             (prefix-regexp (concat semicolons "$" "\\|" semicolons "[^;]")))
        (while (looking-at-p prefix-regexp)
          (forward-line)))
      (msg- (point)))))

(defun lispyy-header-block-bounds ()
  "TODO: documentation"
  (interactive)
  (let ((start-bound (lispyy-beginning-of-header-block-position))
        (end-bound   (lispyy-end-of-header-block-position))
        bounds)
    (if (not (and start-bound end-bound))
        nil
      (setq bounds (cons start-bound end-bound))
      bounds)))

;;;;;
;;;;;  Sections
;;;;;

(defun lispyy-beginning-of-section-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let (start-bound)
      (if (not (lispyy-in-header-block-p))
          (lispyy-header-prev t))
      (setq start-bound (lispyy-beginning-of-header-block-position))
      (msg- start-bound))))

(defun lispyy-end-of-section-position ()
  "TODO: documentation"
  (interactive)
  (save-excursion
    (let (end-bound)
      (if (lispyy-header-next t)
          (setq end-bound (lispyy-beginning-of-header-block-position))
        (setq end-bound (point-max)))
      (msg- end-bound))))

;; (defun lispyy-end-of-section-position ()
;;   "TODO: documentation"
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (if (not (looking-at ";+"))
;;         (msg- nil)
;;       (let* ((semicolons    (match-string 0))
;;              (prefix-regexp (concat semicolons "$" "\\|" semicolons "[^;]")))
;;         (while (save-excursion (let ((pt (point)))
;;                                  (re-search-backward prefix-regexp nil t)
;;                                  (= (1+ (line-number-at-pos))
;;                                     (line-number-at-pos pt))))
;;           (re-search-backward prefix-regexp nil t)))
;;       (msg- (point)))))

(defun lispyy-section-bounds ()
  "TODO: documentation"
  (interactive)
  (let ((start-bound (lispyy-beginning-of-section-position))
        (end-bound   (lispyy-end-of-section-position))
        bounds)
    (if (not (and start-bound end-bound))
        nil
      (setq bounds (cons start-bound end-bound))
      bounds)))

;; (defun lispyy-section-bounds ()
;;   "TODO: documentation"
;;   (interactive)
;;   (let (start-bound
;;         end-bound
;;         bounds)
;;     (when (not (lispyy-in-header-block-p))
;;       (lispyy-header-prev))
;;     (setq start-bound (lispyy-beginning-of-header-block-position))
;;     (lispyy-header-next)
;;     (setq end-bound (lispyy-end-of-header-block-position))
;;     (setq bounds (cons start-bound end-bound))
;;     bounds))

;;;;
;;;;  Header views
;;;;

(defun lispyy-view-header-block-top ()
  "TODO: documentation"
  (interactive)
  (when (lispyy-in-header-block-p)
    (beginning-of-line)
    (looking-at ";+")
    (let* ((pt (point))
           (semicolons (match-string 0))
           (prefix-regexp (concat semicolons "$" "\\|" semicolons "[^;]")))
      (while (save-excursion
               (let ((pt (point)))
                 (re-search-backward prefix-regexp nil t)
                 (= (1+ (line-number-at-pos))
                    (line-number-at-pos pt))))
        (re-search-backward prefix-regexp nil t))
      (recenter 0)
      (goto-char pt)
      (lispyy-beginning-of-header-block-position))))

;;;;
;;;;  Jumping to/from headers
;;;;

(defvar-local lispyy-jump-to-and-from-header-repeat-count 0)
(defvar-local lispyy-jump-to-and-from-header-last-view-position nil)

(defun lispyy-repeated-command-p ()
  "TODO: documentation"
  (eq real-this-command last-command))

(defun lispyy-jump-to-and-from-header (&optional arg)
  "TODO: documentation"
  (interactive)
  (when (not (lispyy-repeated-command-p))
    (setq lispyy-jump-to-and-from-header-repeat-count 0)
    (if (lispyy-in-header-block-p)
        (progn (setq lispyy-jump-to-and-from-header-last-view-position nil)
               (if (not (lispyy-in-header-p))
                   (setq lispyy-jump-to-and-from-header-repeat-count 1)))
      (setq lispyy-jump-to-and-from-header-last-view-position (lispyy-view-position))))
  (setq lispyy-jump-to-and-from-header-repeat-count (1+ lispyy-jump-to-and-from-header-repeat-count))
  (cond ((lispyy-in-header-p)       (if (= (mod lispyy-jump-to-and-from-header-repeat-count 4) 2)
                                        (lispyy-view-position lispyy-jump-to-and-from-header-last-view-position)
                                      (lispyy-jump-from-header)))
        ((lispyy-in-header-block-p) (lispyy-move-to-header))
        (t                          (lispyy-jump-to-header))))

(defun lispyy-jump-to-header (&optional arg)
  "TODO: documentation"
  (interactive)
  (cond ((not (lispyy-in-header-block-p))
         (lispyy-header-prev arg))
        ((lispyy-in-header-p)
         nil)
        ((lispyy-in-header-block-p)
         (lispyy-move-to-header))))

'(lispy-define-key lispy-mode-map "=" 'lispyy-jump-to-and-from-header)

(defun lispyy-move-to-header ()
  "TODO: documentation"
  (interactive)
  (cond ((lispyy-in-header-p) nil)
        ((lispyy-in-header-block-p)
         (cond ((save-excursion
                  (forward-line 1)
                  (lispyy-in-header-p))
                (forward-line 1))
               ((save-excursion
                  (forward-line -1)
                  (lispyy-in-header-p))
                (forward-line -1))))
        (t (lispyy-jump-to-header)))
  (lispyy-view-header-block-top))

(defun lispyy-jump-from-header (&optional arg)
  "TODO: documentation"
  (interactive)
  (when (> (save-excursion
             (re-search-forward hl-function-re nil t)
             (point))
           (save-excursion
             (re-search-forward hl-header-block-re nil t)
             (point)))
    (re-search-forward hl-function-re nil t)
    (beginning-of-line)))

;;;;
;;;;  Next and previous header
;;;;

(defun lispyy-header-next (&optional arg)
  "TODO: documentation"
  (interactive)
  (let ((pt (point)))
    (if (lispyy-in-header-block-p)
        (goto-char (lispyy-end-of-header-block-position)))
    (re-search-forward hl-header-re nil t)
    (beginning-of-line)
    (if (looking-at-p hl-header-re)
        (if (and (not (eq arg t))
                 (= hl-cycle-program-outline-state 0))
            (lispyy-view-header-block-top)
          t)
      (goto-char pt)
      nil)))

(defun lispyy-header-prev (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (re-search-backward hl-header-re nil t)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

(defun lispyy-header-forward (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (lispyy-header-next)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

(defun lispyy-header-backward (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (re-search-backward hl-header-re nil t)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

(defun lispyy-header-up (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (re-search-forward hl-header-re nil t)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

(defun lispyy-header-down (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (re-search-backward hl-header-re nil t)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

(defun lispyy-header-forward-or-down (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (re-search-backward hl-header-re nil t)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

(defun lispyy-header-backward-or-up (&optional arg)
  "TODO: documentation"
  (interactive)
  (if (re-search-forward hl-header-re nil t)
      (if (and (not (eq arg t))
               (= hl-cycle-program-outline-state 0))
          (lispyy-view-header-block-top))
    nil))

;;;;
;;;;  Killing
;;;;

(defun lispyy-kill-section ()
  "TODO: documentation"
  (interactive)
  (if (lispyy-in-header-block-p)
      (let ((bounds (lispyy-section-bounds)))
        (if bounds (kill-region (car bounds) (cdr bounds))))))

;;;
;;;  Utility functions
;;;

(defun delete-other-windows-or-lispy-ace-paren ()
  "TODO: documentation"
  (interactive)
  (if (one-window-p t 1)
      (lispy-ace-paren)
    (delete-other-windows)))

(defun msg (value)
  "TODO: documentation"
  (interactive)
  (let ((string (format "%S" value)))
    (message string))
  value)

(defun msg- (value)
  "TODO: documentation"
  (interactive)
  (let ((string (format "%S" value))
        (inhibit-read-only t))
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (when (not (bolp))
        (insert "\n"))
      (insert string)
      (when (not (bolp))
        (insert "\n"))))
  value)

(defun msg-- (value)
  "TODO: documentation"
  (interactive)
  value)

(provide 'lispyy)
