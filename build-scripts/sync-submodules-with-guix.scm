;; Run with
;;   guix repl -- ./build-scripts/sync-submodules-with-guix.scm

(use-modules (srfi srfi-1)
             (srfi srfi-2)
             (gnu packages web-browsers)
             (guix packages)
             (guix git-download)
             (guix build utils)
             (ice-9 string-fun))

(let* ((cache '())
       (all-deps (lambda (pkg)
                   (if (package? pkg)
                       (delete-duplicates
                        (append (map cadr (package-inputs pkg))
                                (map cadr (package-native-inputs pkg))
                                (map cadr (package-propagated-inputs pkg))))
                       '()))))
  (define (all-recursive-deps pkg)
    (if (and (not (member pkg cache))
             (package? pkg))
        (begin
          (set! cache (cons pkg cache))
          (cons pkg (apply append (map all-recursive-deps (all-deps pkg)))))
        '()))

  (define (git-commit? s)
    (= 40 (string-length s)))

  (define (git-url->name url)
    (string-replace-substring
     (string-replace-substring (basename url) ".git" "") "/" ""))

  (define (sbcl-deps pkg)
    "Return a list of (URL REF).
REF is a commit or a branch name or a tag.
In this context, Git conflates 'branch name' and 'tag' as the same thing."
    (filter (lambda (pkg)
              (and
               (and-let* ((source (package-source pkg))
                          (method (origin-method source))
                          (name (procedure-name method)))
                 (eq? 'git-fetch name))
               (string-prefix? "sbcl-" (package-name pkg))))
            (all-recursive-deps nyxt)))

  (define (package-repo-ref pkg)
    (let* ((origin (origin-uri (package-source pkg)))
           (commit (git-reference-commit origin)))

      (list (git-reference-url origin)
            (git-reference-commit origin)
            (if (git-commit? commit) #:commit #:branch))))

  (define (add-submodule ref)
    (pk ref)
    (let ((path (string-append "_build/"
                               (git-url->name (first ref)))))
      (apply invoke (append '("git" "submodule" "add")
                            (list (first ref) path)))
      (invoke "git" "-C" path "checkout" (second ref))))

  ;; (pk (map package-repo-ref (sbcl-deps nyxt)))
  (map add-submodule (map package-repo-ref (sbcl-deps nyxt))))
