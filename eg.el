(require 'cl-lib)

(defconst eg-magic-ng "NG"
  "Magic marker for a guide built with Norton Guide.")

(defconst eg-magic-eh "EH"
  "Magic marker for a guide built with Expert Help.")

(defconst eg-title-length 40
  "Maximum length of a guide title")

(defconst eg-credit-length 66
  "Maximum length of a line in a guide's credits.")

(defconst eg-entry-short 0
  "Id of a short entry in a guide.")

(defconst eg-entry-long 1
  "Id of a long entry in a guide.")

(defconst eg-entry-menu 2
  "Id of a menu in a guide.")

(defvar eg-buffer-name-function (lambda (file) (format "*EG: %s*" file))
  "Function that names a buffer for reading from a Norton guide file.")

(cl-defstruct eg-guide
  file
  buffer
  magic
  menu-count
  title
  credits
  menus
  (pos 0))

(defun eg-skip (guide bytes)
  "Skip BYTES bytes in GUIDE."
  (cl-incf (eg-guide-pos guide) bytes))

(defun eg-read (guide len)
  "Read LEN bytes from GUIDE."
  (with-current-buffer (eg-guide-buffer guide)
    (setf (buffer-string) "")
    (insert-file-contents-literally
     (eg-guide-file guide) nil (eg-guide-pos guide) (+ (eg-guide-pos guide) len))
    (cl-incf (eg-guide-pos guide) len)
    (buffer-string)))

(defun eg-read-byte (guide &optional decrypt)
  "Read a byte from GUIDE.

If DECRYPT is non-nil, decrypt it."
  (let ((byte (eg-read guide 1)))
    ;; TODO: Decrypt
    (aref byte 0)))

(defun eg-read-word (guide)
  "Read a word from GUIDE."
  (let* ((lo (eg-read-byte guide))
         (hi (eg-read-byte guide)))
    (+ (lsh hi 8)) lo))

(defun eg-read-string (guide len)
  "Read a string of LEN characters from GUIDE.

Any trailing NUL characters are removed."
  (replace-regexp-in-string "\0+$" "" (eg-read guide len)))

(defun eg-read-header (guide)
  "Read the header of GUIDE."
  ;; Read the magic "number".
  (setf (eg-guide-magic guide) (eg-read guide 2))
  ;; Skip 4 bytes (I'm not sure what they are for).
  (eg-skip guide 4)
  ;; Get the count of menus.
  (setf (eg-guide-menu-count guide) (eg-read-word guide))
  ;; Get the title of the guide.
  (setf (eg-guide-title guide) (eg-read-string guide eg-title-length))
  ;; Load the credits for the guide.
  (setf (eg-guide-credits guide)
        (cl-loop for n from 0 to 4
                 collect (eg-read-string guide eg-credit-length)))
  guide)

(defun eg-guide-p (guide)
  "Does GUIDE appear to be a Norton Guide file?"
  (memq (eg-guide-magic guide) (list eg-magic-ng eg-magic-eh)))

(defun eg-guide-has-menus-p (guide)
  "Does GUIDE have menus?"
  (> (eg-guide-menu-count guide) 0))

(defun eg-open (file)
  "Open FILE and return the buffer that'll be used to read it."
  (when (file-exists-p file)
    (eg-read-header
     (with-current-buffer (generate-new-buffer (funcall eg-buffer-name-function file))
       (make-eg-guide :file file :buffer (current-buffer))))))

(defun eg-close (guide)
  "Close GUIDE."
  (kill-buffer (eg-guide-buffer guide)))
