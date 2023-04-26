;; Run with:
;;   guix repl -i -- ./build-scripts/sync-submodules-with-guix.scm
;;   ,m (sync-submodules-with-guix)
;;   (pk (all-refs nyxt))
;;   (map add-submodule (all-refs nyxt))

(define-module (sync-submodules-with-guix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (ice-9 string-fun))

(define (all-deps pkg)
  (if (package? pkg)
      (delete-duplicates
       (append (map cadr (package-inputs pkg))
               (map cadr (package-native-inputs pkg))
               (map cadr (package-propagated-inputs pkg))))
      '()))

(define cache '())
(define (all-recursive-deps pkg)
  (delete-duplicates
   (if (and (not (member pkg cache))
            (package? pkg))
       (begin
         (set! cache (cons pkg cache))
         (cons pkg (apply append (map all-recursive-deps (all-deps pkg)))))
       '())))

(define (git-commit? s)
  (= 40 (string-length s)))

(define (git-url->name url)
  (string-replace-substring
   (string-replace-substring (basename url) ".git" "") "/" ""))

(define (sbcl-deps pkg)
  (filter (lambda (pkg)
            (and
             (and-let* ((source (package-source pkg))
                        (method (origin-method source))
                        (name (procedure-name method)))
               (eq? 'git-fetch name))
             (string-prefix? "sbcl-" (package-name pkg))))
          (all-recursive-deps nyxt)))

(define (package-repo-ref pkg)
  "Return a list of (URL REF).
REF is a commit or a branch name or a tag.
In this context, Git conflates 'branch name' and 'tag' as the same thing."
  (let* ((origin (origin-uri (package-source pkg)))
         (commit (git-reference-commit origin)))

    (list (git-reference-url origin)
          (git-reference-commit origin))))

(define (all-refs pkg)
  "Return all unique repo refs of PKG."
  (delete-duplicates (map package-repo-ref (sbcl-deps pkg))))

(define (add-submodule ref)
  (pk ref)
  (let ((path (string-append "_build/"
                             (git-url->name (first ref)))))
    (let ((code
           (with-error-to-port (%make-void-port "w")
             (lambda ()
               (system* "git" "submodule" "status" path)))))
      (unless (zero? code)
        (apply invoke (append '("git" "submodule" "add")
                              (list (first ref) path)))
        (invoke "git" "-C" path "checkout" (second ref))))))
