;;; spaced.el --- -*- lexical-binding: t -*-

(setq spaced-vectors-alist
      '((nil . (
                [":" ": "]
                ["::" "::"]
                ["=" " = "]
                ["==" " == "]
                ["===" " === "]
                ["||" " || "]
                ["||=" " ||= "]
                ))))

(defun spaced-compare-char-frequencies (str1 str2)
  (equal (sort (append str1 nil) '<)
         (sort (append str2 nil) '<)))

(defun spaced-remove-extra-spaces (str)
  (replace-regexp-in-string
   "\\` *\\(.*?\\) *\\'"  ;; Match leading/trailing spaces
   (lambda (m) (concat (if (string-prefix-p " " m) " " "")  ;; Keep at most one leading space
                       (replace-regexp-in-string " " "" (match-string 1 m))  ;; Remove inner spaces
                       (if (string-suffix-p " " m) " " ""))) ;; Keep at most one trailing space
   str))

(defun spaced-vectors-for-mode ()
  (catch 'found
    (dolist (pair spaced-vectors-alist)
      (when (or (derived-mode-p (car pair)) (eq nil (car pair)))
        (throw 'found (cdr pair))))))

(defun spaced-post-self-insert-function ()
  (let* ((start (max (line-beginning-position) (- (point) 7)))
         (str (spaced-remove-extra-spaces (buffer-substring-no-properties start (point))))
         (vectors (spaced-vectors-for-mode))
         (matches nil))
    (dolist (vector vectors)
      (let ((trigger (aref vector 0))
            (template (aref vector 1)))
        (when (string-suffix-p trigger str)
          (push vector matches))))
    (when-let* ((longest-match (pop matches)))
      (let* ((trigger (aref longest-match 0))
             (template (aref longest-match 1))
             (delete-length (length trigger))
             (template-length (length template))
             (str (buffer-substring-no-properties (- (point) delete-length) (point))))

        (when (not (string= str trigger))
          (if (> template-length delete-length)
              (let ((count template-length))
                (catch 'break
                  (while (> count (1- delete-length))
                    (when (spaced-compare-char-frequencies
                           (buffer-substring-no-properties (- (point) count) (point))
                           template)
                      (setq delete-length count)
                      (throw 'break "adjusted delete length"))
                    (setq count (1- count)))))
            (let ((count 2))
              (catch 'break
                (while (> count 0)
                  (when (spaced-compare-char-frequencies
                         (buffer-substring-no-properties (- (point) (+ delete-length count)) (point))
                         (concat template (make-string count ?\s)))
                    (setq delete-length (+ count delete-length))
                    (throw 'break "adjusted delete length"))
                  (setq count (1- count)))))))

        (let ((delete-point (- (point) delete-length))
              (goggles-pulse nil))
          (when (not (string= template (buffer-substring-no-properties delete-point (point))))
            (undo-boundary)
            (delete-region delete-point (point))
            (insert template)))))))

(define-minor-mode spaced-mode
  ""
  :global t
  (if spaced-mode
      (add-hook 'post-self-insert-hook #'spaced-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook #'spaced-post-self-insert-function t)))
