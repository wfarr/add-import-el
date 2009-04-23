;;; Original Author: Jonathon Rockway

(require 'thingatpt)

(defvar add-import-languages
  '(
    ;; (lang . '(finder getter keyword wrapper ending_punctuation))
    (perl . ("[:alpha:]:\\->" "[[:alpha:]:]+" "use" nil ";"))
    (ruby . ("[:alpha:]:" "[[:alpha:]:]+" "require" "'" nil))
    )
  )

(defun find-language ()
  ;; check major mode
  (cond (major-mode
         (let* ((current-mode (symbol-name major-mode)))
           (string-match "^\\([[:alpha:]-]+\\)\\-mode" current-mode)
           (match-string 1 current-mode))))
;;TODO
;; check file extension
;; check shebang
)

(defun bounds-of-module-at-point ()
  "Determine where a module name starts for (thing-at-point 'perl-module)"
  (save-excursion
    (skip-chars-backward "[:alpha:]:\\->")  ; skip to F in Foo::Bar->
    (if (looking-at "[[:alpha:]:]+")        ; then get Foo::Bar
          (cons (point) (match-end 0))
      nil)))
    

; allow (thing-at-point 'perl-module)
(put 'perl-module 'bounds-of-thing-at-point 'bounds-of-module-at-point)

(defun read-with-default (string &optional default error)
  (let ((read (read-string
               (if default 
                   (format "%s (default %s): " string default)
                 (format "%s: " string)))))
    (if (equal read "") (setq read nil))
    (if (and (not read) (not default)) (error error))
    (if (not read) (setq read default))
    read))
    

(defun add-semicolon (string)
  (if (string-match ";" string)
      string
    (concat string ";")))
    

(defun add-use ()
  "Add a new perl use statement after the existing use statements."
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module to use!")))
    (save-excursion
      (goto-char (point-max))
      (condition-case nil
          (re-search-backward "^\\(use .+;\\)")
        (error (goto-char 0)))
      (end-of-line)
      (insert (concat "\nuse " (add-semicolon module))))))
