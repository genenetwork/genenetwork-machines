;;; genenetwork-machines --- Guix configuration for genenetwork machines
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Frederick Muriuki Muriithi <fredmanglis@gmail.com>
;;;
;;; This file is part of genenetwork-machines.
;;;
;;; genenetwork-machines is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; genenetwork-machines is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with genenetwork-machines.  If not, see
;;; <https://www.gnu.org/licenses/>.

(use-modules (gnu)
             ((gn packages genenetwork) #:select (genenetwork2 genenetwork3))
             ((gn packages quality-control) #:select (sbcl-qc))
             (gn services databases)
             (gnu build linux-container)
             ((gnu packages admin) #:select (shepherd shadow))
             ((gnu packages base) #:select (gnu-make tar))
             ((gnu packages bash) #:select (bash))
             ((gnu packages bioinformatics) #:select (ccwl) #:prefix guix:)
             ((gnu packages certs) #:select (nss-certs))
             ((gnu packages check) #:select (python-pylint))
             ((gnu packages ci) #:select (laminar))
             ((gnu packages compression) #:select (gzip))
             ((gnu packages databases) #:select (virtuoso-ose))
             ((gnu packages gnupg) #:select (guile-gcrypt))
             ((gnu packages graphviz) #:select (graphviz))
             ((gnu packages guile) #:select (guile-3.0 guile-zlib))
             ((gnu packages guile-xyz) #:select (guile-dbd-mysql guile-dbi guile-hashing guile-libyaml))
             ((gnu packages guile-xyz) #:select (guile-sparql) #:prefix guix:)
             ((gnu packages haskell-apps) #:select (shellcheck))
             ((gnu packages python-check) #:select (python-mypy))
             ((gnu packages python-web) #:select (gunicorn))
             ((gnu packages rdf) #:select (raptor2))
             ((gnu packages tls) #:select (openssl))
             ((gnu packages version-control) #:select (git-minimal))
             (gnu services ci)
             (gnu services databases)
             (gnu services mcron)
             (gnu services shepherd)
             (gnu services web)
             (gnu system file-systems)
             (guix build-system gnu)
             (guix channels)
             (guix git-download)
             (guix least-authority)
             ((guix licenses) #:prefix license:)
             (guix modules)
             (guix packages)
             (guix profiles)
             (guix records)
             (guix store)
             (forge forge)
             (forge laminar)
             (forge socket)
             (forge tissue)
             (forge utils)
             (forge webhook)
             (srfi srfi-1)
             (ice-9 match))

;; guix-daemon socket of the host shared inside the container
(define %guix-daemon-uri
  "/var/host-guix/daemon-socket/socket")

;; We cannot refer to sudo in the store since that sudo does not have
;; the setuid bit set. See "(guix) Setuid Programs".
(define sudo
  "/run/setuid-programs/sudo")

(define (manifest-cons package onto-manifest)
  "Return a manifest with PACKAGE and all packages in ONTO-MANIFEST."
  (manifest (cons (package->manifest-entry package)
                  (manifest-entries onto-manifest))))

(define (manifest-cons* . args)
  "ARGS is of the form (PACKAGES ... ONTO-MANIFEST). Return a manifest
with PACKAGES and all packages in ONTO-MANIFEST."
  (let ((packages (drop-right args 1))
        (onto-manifest (last args)))
    (manifest (append (map package->manifest-entry packages)
                      (manifest-entries onto-manifest)))))

(define (import-module? name)
  "Return #t if NAME, a list of symbols, denotes a module that should
be imported into G-expressions."
  ;; Allow all guix-forge, genenetwork or guix modules.
  (match name
    (((or 'forge 'genenetwork) _ ...) #t)
    (name (guix-module-name? name))))

(define-record-type* <genenetwork-configuration>
  genenetwork-configuration make-genenetwork-configuration
  genenetwork-configuration?
  (gn2-repository genenetwork-configuration-gn2-repository
                  (default "https://github.com/genenetwork/genenetwork2"))
  (gn3-repository genenetwork-configuration-gn3-repository
                  (default "https://github.com/genenetwork/genenetwork3"))
  (gn2-port genenetwork-configuration-gn2-port
            (default 8082))
  (gn3-port genenetwork-configuration-gn3-port
            (default 8083))
  (genotype-files genenetwork-configuration-genotype-files
                  (default "/var/genenetwork/genotype-files"))
  (sparql-endpoint genenetwork-configuration-sparql-endpoint
                   (default "http://localhost:8081/sparql"))
  (xapian-db-path genenetwork-xapian-db-path
                  (default "/var/genenetwork/xapian")))


;;;
;;; genenetwork
;;;

(define (genenetwork2-tests config test-command)
  "Return a G-expression that runs TEST-COMMAND for genenetwork2
described by CONFIG, a <genenetwork-configuration>
object. TEST-COMMAND is a list of strings specifying the command to be
executed."
  (match-record config <genenetwork-configuration>
    (gn2-repository gn3-repository gn3-port genotype-files)
    (with-imported-modules '((guix build utils))
      (with-packages (list bash coreutils git-minimal nss-certs)
        #~(begin
            (use-modules (guix build utils))

            (define (hline)
              "Print a horizontal line 50 '=' characters long."
              (display (make-string 50 #\=))
              (newline)
              (force-output))

            (define (show-head-commit)
              (hline)
              (invoke "git" "log" "--max-count" "1")
              (hline))

            (invoke "git" "clone" "--depth" "1" #$gn3-repository)
            (with-directory-excursion "genenetwork3"
              (show-head-commit))
            (invoke "git" "clone" "--depth" "1" #$gn2-repository)
            (with-directory-excursion "genenetwork2"
              (show-head-commit))
            ;; This is a dummy SERVER_PORT to placate
            ;; bin/genenetwork2. TODO: Fix bin/genenetwork2 so that
            ;; this is not needed.
            (setenv "SERVER_PORT" "8080")
            ;; Use a profile with all dependencies except
            ;; genenetwork3.
            (setenv "GN2_PROFILE"
                    #$(profile
                       (content (package->development-manifest genenetwork2))
                       (allow-collisions? #t)))
            ;; Set GN3_PYTHONPATH to the latest genenetwork3.
            (setenv "GN3_PYTHONPATH"
                    (string-append (getcwd) "/genenetwork3"))
            (setenv "GN_PROXY_URL" "http://genenetwork.org/gn3-proxy/")
            (setenv "GN3_LOCAL_URL" (string-append "http://localhost:" (number->string #$gn3-port)))
            (setenv "GENENETWORK_FILES" #$genotype-files)
            (setenv "HOME" "/tmp")
            (setenv "SQL_URI" "mysql://webqtlout:webqtlout@localhost/db_webqtl")
            (chdir "genenetwork2")
            (apply invoke '#$test-command))))))

(define (genenetwork3-tests tests-command manifest)
  "Return a G-expression running TESTS-COMMAND in a profile described
by MANIFEST with the latest git checkout of genenetwork3 as the
current directory. TESTS-COMMAND is a list of strings specifying the
command to be executed."
  (with-imported-modules (source-module-closure '((genenetwork development-helper))
                                                #:select? import-module?)
    #~(lambda (genenetwork3-checkout)
        ((@@ (genenetwork development-helper)
             command-in-source-gexp)
         genenetwork3-checkout
         #$(profile
            (content manifest)
            (allow-collisions? #t))
         '#$tests-command))))

(define genenetwork3-unit-tests
  (genenetwork3-tests (list "pytest" "-k" "unit_test")
                      (package->development-manifest genenetwork3)))

(define genenetwork3-pylint
  (with-imported-modules (source-module-closure '((genenetwork development-helper))
                                                #:select? import-module?)
    #~(lambda (genenetwork3-checkout)
        ((@@ (genenetwork development-helper)
             genenetwork3-lint-gexp)
         genenetwork3-checkout
         #$(profile
            (content (manifest-cons* python-pylint shellcheck
                                     (package->development-manifest genenetwork3)))
            (allow-collisions? #t))))))

(define genenetwork3-mypy
  (genenetwork3-tests (list "mypy" ".")
                      (manifest-cons python-mypy
                                     (package->development-manifest genenetwork3))))

(define %xapian-directory
  "/export/data/genenetwork/xapian")

(define (build-xapian-index-gexp project)
  "Return a G-expression that builds and installs a Xapian index using
genenetwork3 source from the latest commit of @var{project}."
  (with-imported-modules '((guix build utils))
    (with-manifest (manifest-cons* git-minimal nss-certs
                                   (package->development-manifest genenetwork3))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-26))

          (invoke "git" "clone" "--depth" "1"
                  #$(forge-project-repository project)
                  ".")
          (let ((xapian-directory #$%xapian-directory)
                (xapian-build-directory (string-append #$%xapian-directory
                                                       "/build")))
            (dynamic-wind
              (cut mkdir xapian-build-directory)
              (lambda ()
                ;; Build xapian index.
                (setenv "PYTHONPATH" (getcwd))
                (invoke "./scripts/index-genenetwork" xapian-build-directory)
                ;; Stop genenetwork3, replace old xapian index and
                ;; start genenetwork3.
                (dynamic-wind
                  (cut invoke #$sudo #$(file-append shepherd "/bin/herd") "stop" "genenetwork3")
                  (lambda ()
                    (for-each (lambda (file)
                                (rename-file file (string-append xapian-directory "/" (basename file))))
                              (find-files xapian-build-directory)))
                  (cut invoke #$sudo #$(file-append shepherd "/bin/herd") "start" "genenetwork3")))
              (cut delete-file-recursively xapian-build-directory)))))))

(define (genenetwork-projects config)
  "Return forge projects for genenetwork described by CONFIG, a
<genenetwork-configuration> object."
  (match-record config <genenetwork-configuration>
    (gn2-repository gn3-repository gn2-port)
    (list (forge-project
           (name "genenetwork2")
           (repository gn2-repository)
           (ci-jobs (list (forge-laminar-job
                           (name "genenetwork2")
                           (run (genenetwork2-tests
                                 config
                                 (list "sh" "bin/genenetwork2" "./etc/default_settings.py"
                                       "-c" "-m" "pytest")))
                           ;; If unit tests pass, redeploy genenetwork2 and
                           ;; trigger Mechanical Rob.
                           (after (with-imported-modules '((guix build utils))
                                    #~(begin
                                        (use-modules (guix build utils))
                                        (when (string=? (getenv "RESULT") "success")
                                          (invoke #$sudo
                                                  #$(file-append shepherd "/bin/herd")
                                                  "restart" "genenetwork2")
                                          (invoke #$(file-append laminar "/bin/laminarc")
                                                  "queue" "genenetwork2-mechanical-rob"))))))
                          (forge-laminar-job
                           (name "genenetwork2-mechanical-rob")
                           (run (genenetwork2-tests
                                 config
                                 (list "sh" "bin/genenetwork2" "./etc/default_settings.py"
                                       "-c" "../test/requests/test-website.py"
                                       "--all" (string-append "http://localhost:" (number->string gn2-port)))))
                           (trigger? #f))))
           (ci-jobs-trigger 'webhook))
          (forge-project
           (name "genenetwork3")
           (repository gn3-repository)
           (ci-jobs (list (forge-laminar-job
                           (name "genenetwork3")
                           (run (derivation-job-gexp
                                 this-forge-project
                                 this-forge-laminar-job
                                 genenetwork3-unit-tests
                                 #:guix-daemon-uri %guix-daemon-uri))
                           ;; If unit tests pass, redeploy genenetwork3 and
                           ;; trigger genenetwork2 tests.
                           (after (with-imported-modules '((guix build utils))
                                    #~(begin
                                        (use-modules (guix build utils))
                                        (when (string=? (getenv "RESULT") "success")
                                          (invoke #$sudo
                                                  #$(file-append shepherd "/bin/herd")
                                                  "restart" "genenetwork3")
                                          (invoke #$(file-append laminar "/bin/laminarc")
                                                  "queue" "genenetwork2"))))))
                          (forge-laminar-job
                           (name "genenetwork3-pylint")
                           (run (derivation-job-gexp
                                 this-forge-project
                                 this-forge-laminar-job
                                 genenetwork3-pylint
                                 #:guix-daemon-uri %guix-daemon-uri)))
                          (forge-laminar-job
                           (name "genenetwork3-mypy")
                           (run (derivation-job-gexp
                                 this-forge-project
                                 this-forge-laminar-job
                                 genenetwork3-mypy
                                 #:guix-daemon-uri %guix-daemon-uri)))
                          (forge-laminar-job
                           (name "genenetwork3-build-xapian-index")
                           (run (build-xapian-index-gexp this-forge-project))
                           (trigger? #f))))
           (ci-jobs-trigger 'webhook)))))

(define (genenetwork2-cd-gexp config)
  "Return a G-expression that runs the latest genenetwork2 development
server described by CONFIG, a <genenetwork-configuration> object."
  (match-record config <genenetwork-configuration>
    (gn2-repository gn3-repository gn2-port gn3-port genotype-files)
    (with-packages (list coreutils git-minimal gunicorn nss-certs)
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 match))

            (define (hline)
              "Print a horizontal line 50 '=' characters long."
              (display (make-string 50 #\=))
              (newline)
              (force-output))

            (define (show-head-commit)
              (hline)
              (invoke "git" "log" "--max-count" "1")
              (hline))

            ;; Clone the latest genenetwork2 and genenetwork3
            ;; repositories.
            (invoke "git" "clone" "--depth" "1" #$gn2-repository)
            (with-directory-excursion "genenetwork2"
              (show-head-commit))
            (invoke "git" "clone" "--depth" "1" #$gn3-repository)
            (with-directory-excursion "genenetwork3"
              (show-head-commit))

            ;; Override the genenetwork3 used by genenetwork2.
            (setenv "GN3_PYTHONPATH"
                    (string-append (getcwd) "/genenetwork3"))
            ;; Set other environment variables required by
            ;; genenetwork2.
            (setenv "SERVER_PORT" #$(number->string gn2-port))
            (setenv "GN2_PROFILE" #$(profile
                                     (content (package->development-manifest genenetwork2))
                                     (allow-collisions? #t)))
            (setenv "GN_PROXY_URL" "http://genenetwork.org/gn3-proxy/")
            (setenv "GN_SERVER_URL" "/api3")
            (setenv "GN3_LOCAL_URL"
                    #$(string-append "http://localhost:"
                                     (number->string gn3-port)))
            (setenv "GENENETWORK_FILES" #$genotype-files)
            (setenv "SQL_URI" "mysql://webqtlout:webqtlout@localhost/db_webqtl")
            (setenv "HOME" "/tmp")
            (setenv "NO_REDIS" "no-redis")
	    (setenv "RUST_BACKTRACE" "1")

            ;; Start genenetwork2.
            (with-directory-excursion "genenetwork2"
              (invoke #$(file-append bash "/bin/sh")
                      "bin/genenetwork2" "etc/default_settings.py" "-gunicorn-prod")))))))

(define (genenetwork3-cd-gexp config)
  "Return a G-expression that runs the latest genenetwork3 development
server described by CONFIG, a <genenetwork-configuration> object."
  (match-record config <genenetwork-configuration>
    (gn3-repository gn3-port sparql-endpoint xapian-db-path)
    (with-manifest (package->development-manifest genenetwork3)
      (with-packages (list git-minimal nss-certs)
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils)
                           (ice-9 match))

              (define (hline)
                "Print a horizontal line 50 '=' characters long."
                (display (make-string 50 #\=))
                (newline)
                (force-output))

              (define (show-head-commit)
                (hline)
                (invoke "git" "log" "--max-count" "1")
                (hline))

              ;; Clone the latest genenetwork3 repository.
              (invoke "git" "clone" "--depth" "1" #$gn3-repository)
              ;; Configure genenetwork3.
              (setenv "GN3_CONF"
                      #$(mixed-text-file "gn3.conf"
                                         "SPARQL_ENDPOINT=\"" sparql-endpoint "\"\n"
                                         "XAPIAN_DB_PATH=\"" xapian-db-path "\"\n"))
              (setenv "HOME" "/tmp")
              ;; Run genenetwork3.
              (with-directory-excursion "genenetwork3"
                (show-head-commit)
                (invoke #$(file-append gunicorn "/bin/gunicorn")
                        "-b" #$(string-append "localhost:" (number->string gn3-port))
                        "gn3.app:create_app()"))))))))

(define (genenetwork-shepherd-services config)
  "Return shepherd services to run the genenetwork development server
described by CONFIG, a <genenetwork-configuration> object."
  (match-record config <genenetwork-configuration>
    (gn2-port gn3-port genotype-files xapian-db-path)
    (list (shepherd-service
           (documentation "Run GeneNetwork 2 development server.")
           (provision '(genenetwork2))
           ;; FIXME: The genenetwork2 service should depend on redis.
           (requirement '(networking genenetwork3))
           (start #~(make-forkexec-constructor
                     (list #$(least-authority-wrapper
                              (program-file "genenetwork2"
                                            (genenetwork2-cd-gexp config))
                              #:name "genenetwork2-pola-wrapper"
                              #:mappings (list (file-system-mapping
                                                (source genotype-files)
                                                (target source))
                                               (file-system-mapping
                                                (source "/run/mysqld/mysqld.sock")
                                                (target source)
                                                (writable? #t)))
                              #:namespaces (delq 'net %namespaces))
                           "127.0.0.1" #$(number->string gn2-port))
                     #:user "genenetwork"
                     #:group "genenetwork"
                     #:log-file "/var/log/cd/genenetwork2.log"))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (documentation "Run GeneNetwork 3 development server.")
           (provision '(genenetwork3))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list #$(least-authority-wrapper
                              (program-file "genenetwork3"
                                            (genenetwork3-cd-gexp config))
                              #:name "genenetwork3-pola-wrapper"
                              #:mappings (list (file-system-mapping
                                                (source "/run/mysqld/mysqld.sock")
                                                (target source)
                                                (writable? #t))
                                               (file-system-mapping
                                                (source xapian-db-path)
                                                (target source)))
                              #:namespaces (delq 'net %namespaces))
                           "127.0.0.1" #$(number->string gn3-port))
                     #:user "genenetwork"
                     #:group "genenetwork"
                     #:log-file "/var/log/cd/genenetwork3.log"))
           (stop #~(make-kill-destructor))))))

(define %genenetwork-accounts
  (list (user-group
         (name "genenetwork")
         (system? #t))
        (user-account
         (name "genenetwork")
         (group "genenetwork")
         (system? #t)
         (comment "GeneNetwork user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define genenetwork-service-type
  (service-type
   (name 'genenetwork)
   (description "Run GeneNetwork development servers and CI.")
   (extensions
    (list (service-extension account-service-type
                             (const %genenetwork-accounts))
          (service-extension shepherd-root-service-type
                             genenetwork-shepherd-services)
          (service-extension forge-service-type
                             genenetwork-projects)))
   (default-value (genenetwork-configuration))))


;;;
;;; qc
;;; 

(define qc-tests
  (with-imported-modules (cons '(guix build lisp-utils)
                               (source-module-closure '((genenetwork development-helper))
                                                      #:select? import-module?))
    #~(lambda (source)
        ((@@ (genenetwork development-helper)
             asdf-test-gexp)
         source
         #$(profile
            (content (package->development-manifest sbcl-qc))
            (allow-collisions? #t))
         "qc" (list "qc.asd")))))

(define qc-project
  (forge-project
   (name "qc")
   (repository "https://git.genenetwork.org/jgart/qc")
   (ci-jobs (list (forge-laminar-job
                   (name "qc")
                   (run (derivation-job-gexp
                         this-forge-project
                         this-forge-laminar-job
                         qc-tests
                         #:guix-daemon-uri %guix-daemon-uri)))))
   (ci-jobs-trigger 'webhook)))


;;;
;;; dump-genenetwork-database
;;; 

;; Path to genenetwork database dump export directory that has lots of
;; free space
(define %dump-genenetwork-database-export-directory
  "/export/genenetwork-database-dump")

;; Unreleased version of ccwl that is required by
;; dump-genenetwork-database for its graphql library.
(define ccwl
  (let ((commit "02677a508b407779f5991a230341e016deb7f69b")
        (revision "0"))
    (package
      (inherit guix:ccwl)
      (name "ccwl")
      (version (git-version (package-version guix:ccwl) revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arunisaac/ccwl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1kxry8y0pibl0x789jrzqkkh2s59ajyinfvrgvd00gkbqldih82r")))))))

;; guile-sparql tests are broken. Disable them temporarily. The issue
;; has been reported upstream at
;; https://github.com/roelj/guile-sparql/issues/6
(define guile-sparql
  (package
    (inherit guix:guile-sparql)
    (arguments
     `(#:tests? #f))))

;; Temporarily package run64 here until it can be contributed to
;; upstream Guix.
(define run64
  (package
    (name "run64")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.systemreboot.net/run64")
                    (commit "a271723e3938b158aa6748e8befceb114b84c6df")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kdx8h2x5a4cp8azzy1v2xgyb2153sakb1wbi2rj5ygkpmasygbm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://run64.systemreboot.net")
    (synopsis "SRFI-64 test runner for Scheme")
    (description "run64 is a SRFI-64 test runner for Scheme.")
    (license license:gpl3+)))

(define dump-genenetwork-database-tests
  (with-imported-modules (source-module-closure '((genenetwork development-helper))
                                                #:select? import-module?)
    #~(lambda (source)
        ((@@ (genenetwork development-helper)
             command-in-source-gexp)
         source
         #$(profile
            (content (packages->manifest
                      (list gnu-make guile-3.0 guile-dbi guile-dbd-mysql
                            ccwl guile-libyaml guile-sparql run64)))
            (allow-collisions? #t))
         (list "make" "check")))))

(define (dump-genenetwork-database project)
  (with-imported-modules '((guix build utils))
    (with-packages (list ccwl git-minimal gnu-make guile-3.0 guile-dbd-mysql
                         guile-dbi guile-hashing guile-libyaml guile-sparql
                         guile-zlib nss-certs virtuoso-ose)
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-26)
                       (ice-9 threads))

          (invoke "git" "clone"
                  "--depth" "1"
                  #$(forge-project-repository project)
                  ".")
          (invoke "make" "-j" (number->string (current-processor-count)))
          (let ((connection-settings-file #$(string-append %dump-genenetwork-database-export-directory
                                                           "/conn.scm"))
                (dump-directory #$(string-append %dump-genenetwork-database-export-directory
                                                 "/dump")))
            (when (file-exists? dump-directory)
              (delete-file-recursively dump-directory))
            (mkdir-p dump-directory)
            ;; Dump data to RDF.
            (invoke "./pre-inst-env" "./dump.scm"
                    connection-settings-file
                    dump-directory)
            ;; Validate dumped RDF, sending the error output to
            ;; oblivion because we don't want to print out potentially
            ;; sensitive data.
            (with-error-to-file "/dev/null"
              (cut invoke
                   #$(file-append raptor2 "/bin/rapper")
                   "--input" "turtle"
                   "--count"
                   (string-append dump-directory "/dump.ttl")))
            ;; Load RDF into virtuoso.
            (invoke "./pre-inst-env" "./load-rdf.scm"
                    connection-settings-file
                    (string-append dump-directory "/dump.ttl"))
            ;; Visualize schema and archive results.
            (invoke "./pre-inst-env" "./visualize-schema.scm"
                    connection-settings-file)
            (invoke #$(file-append graphviz "/bin/dot")
                    "-Tsvg" "sql.dot" (string-append "-o" (getenv "ARCHIVE") "/sql.svg"))
            (invoke #$(file-append graphviz "/bin/dot")
                    "-Tsvg" "rdf.dot" (string-append "-o" (getenv "ARCHIVE") "/rdf.svg")))))))

(define dump-genenetwork-database-project
  (forge-project
   (name "dump-genenetwork-database")
   (repository "https://github.com/genenetwork/dump-genenetwork-database")
   (ci-jobs (list (forge-laminar-job
                   (name "dump-genenetwork-database-tests")
                   (run (derivation-job-gexp
                         this-forge-project
                         this-forge-laminar-job
                         dump-genenetwork-database-tests
                         #:guix-daemon-uri %guix-daemon-uri)))
                  (forge-laminar-job
                   (name "dump-genenetwork-database")
                   (run (dump-genenetwork-database this-forge-project)))))
   (ci-jobs-trigger 'webhook)))


;;;
;;; gn-gemtext-threads
;;;

(define gn-gemtext-threads-project
  (forge-project
   (name "gn-gemtext-threads")
   (repository "https://github.com/genenetwork/gn-gemtext-threads/")
   (ci-jobs (list (forge-laminar-job
                   (name "gn-gemtext-threads")
                   (run (with-packages (list nss-certs openssl)
                          (with-imported-modules '((guix build utils))
                            #~(begin
                                (use-modules (guix build utils))

                                (invoke #$(file-append tissue "/bin/tissue")
                                        "pull" "issues.genenetwork.org"))))))))
   (ci-jobs-trigger 'webhook)))


;;;
;;; operating-system definition
;;;

(define (laminar-template-gexp issue-tracker-uri)
  "Return a G-expression that creates a custom Laminar template with a
menu link to channels.scm and the issue tracker at ISSUE-TRACKER-URI."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (copy-file (string-append #$(package-source laminar) "/src/resources/index.html")
                   #$output)
        (substitute* #$output
          (("<router-link to=\"jobs\">Jobs</router-link>" jobs-link)
           (string-append
            "<a href=\"https://cd.genenetwork.org\" target=\"_blank\">CD</a>"
            jobs-link
            "<a href=\"" #$issue-tracker-uri "\" target=\"_blank\">Issues</a>"
            "<a href=\"/channels.scm\" target=\"_blank\">channels.scm</a>"))))))

(define (install-laminar-template-gexp template)
  "Return a G-expression that installs custom laminar TEMPLATE."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define (switch-symlinks link target)
          (let ((pivot (string-append link ".new")))
            (symlink target pivot)
            (rename-file pivot link)))

        (mkdir-p "/var/lib/laminar/custom")
        (switch-symlinks "/var/lib/laminar/custom/index.html" #$template))))

(define (channels-scm-gexp published-channel-names)
  "Return a G-expression that builds a directory with a channels.scm
file to be served by the laminar reverse
proxy. PUBLISHED-CHANNEL-NAMES is a list of names of channels which
should be included in the channels.scm file."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (ice-9 pretty-print)
                     (guix build utils))

        (mkdir-p #$output)
        (call-with-output-file (string-append #$output "/channels.scm")
          (lambda (port)
            (pretty-print
             '#$`(list ,@(filter-map (lambda (channel)
                                       (and (memq (channel-name channel)
                                                  published-channel-names)
                                            (channel->code channel)))
                                     (profile-channels
                                      (or (getenv "GUIX_PROFILE")
                                          (string-append %profile-directory "/current-guix")))))
             port))))))

(define (development-server-reverse-proxy-server-block listen gn2-port gn3-port)
  "Return an <nginx-server-configuration> object listening on LISTEN to
reverse proxy the GeneNetwork development server. GN2-PORT and
GN3-PORT are the ports GeneNetwork2 and GeneNetwork3 are listening
on."
  (nginx-server-configuration
   (server-name '("cd.genenetwork.org"))
   (listen (list listen))
   (locations
    (list (nginx-location-configuration
           ;; Reverse proxy genenetwork2.
           (uri "/")
           (body (list (string-append "proxy_pass http://localhost:"
                                      (number->string gn2-port) ";")
                       "proxy_set_header Host $host;")))
          (nginx-location-configuration
           ;; Reverse proxy genenetwork3.
           (uri "/api3")
           (body (list "rewrite /api3/(.*) /api/$1 break;"
                       (string-append "proxy_pass http://localhost:"
                                      (number->string gn3-port) ";")
                       "proxy_set_header Host $host;")))))))

(define (laminar-reverse-proxy-server-block listen laminar-bind-http webhook-port published-channel-names)
  "Return an <nginx-server-configuration> object to reverse proxy
laminar. The nginx server will listen on LISTEN and reverse proxy to
laminar listening on LAMINAR-BIND-HTTP. WEBHOOK-PORT is the port the
webhook server is listening on. PUBLISHED-CHANNEL-NAMES is a list of
channel names for which a channels.scm should be published."
  (nginx-server-configuration
   (server-name '("ci.genenetwork.org"))
   (listen (list listen))
   (locations
    (list (nginx-location-configuration
           (uri "/")
           (body (list (string-append "proxy_pass http://" laminar-bind-http ";")
                       ;; Disable proxy buffering in host's nginx. We
                       ;; need this to allow Laminar's Server-Sent
                       ;; Events to pass through.
                       "proxy_pass_header X-Accel-Buffering;")))
          ;; Reverse proxy webhook server.
          (nginx-location-configuration
           (uri "/hooks/")
           (body (list (string-append "proxy_pass http://localhost:"
                                      (number->string webhook-port) ";")
                       "proxy_set_header Host $host;")))
          ;; Publish the channels.scm used to build this container.
          (nginx-location-configuration
           (uri "= /channels.scm")
           (body (list #~(string-append
                          "root "
                          #$(computed-file "channels.scm"
                                           (channels-scm-gexp published-channel-names))
                          ";"))))))))

(define (tissue-reverse-proxy-server-block listen)
  "Return an <nginx-server-configuration> object listening on LISTEN to
reverse proxy tissue."
  (nginx-server-configuration
   (server-name '("issues.genenetwork.org"))
   (listen (list listen))
   (root "/var/lib/tissue/issues.genenetwork.org/website")
   (try-files (list "$uri" "$uri.html" "@tissue-search"))
   (locations
    (list (nginx-location-configuration
           (uri "@tissue-search")
           (body (list "proxy_pass http://unix:/var/run/tissue/socket:;"
                       "proxy_set_header Host $host;")))))))

;; Port on which webhook is listening
(define %webhook-port 9091)
;; Port on which genenetwork2 is listening
(define %genenetwork2-port 9092)
;; Port on which genenetwork3 is listening
(define %genenetwork3-port 9093)
;; Port on which virtuoso's SPARQL endpoint is listening
(define %virtuoso-sparql-port 9082)

(operating-system
  (host-name "genenetwork-development")
  (timezone "UTC")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sdX"))))
  (file-systems %base-file-systems)
  (users %base-user-accounts)
  (packages %base-packages)
  (sudoers-file
   (mixed-text-file "sudoers"
                    "@include " %sudoers-specification
                    ;; Permit the laminar user to restart genenetwork2
                    ;; and genenetwork3.
                    "\nlaminar ALL = NOPASSWD: "
                    (file-append shepherd "/bin/herd") " restart genenetwork2, "
                    (file-append shepherd "/bin/herd") " start genenetwork3, "
                    (file-append shepherd "/bin/herd") " stop genenetwork3, "
                    (file-append shepherd "/bin/herd") " restart genenetwork3\n"))
  (services (cons* (service forge-service-type
                            (forge-configuration
                             (projects (list qc-project
                                             dump-genenetwork-database-project
                                             gn-gemtext-threads-project))))
                   (service laminar-service-type
                            (laminar-configuration
                             (title "GeneNetwork CI")
                             (bind-http "localhost:9089")))
                   (service mcron-service-type
                            (mcron-configuration
                             (jobs (list #~(job '(next-day)
                                                #$(program-file "build-xapian-index-cron"
                                                                (with-imported-modules '((guix build utils))
                                                                  #~(begin
                                                                      (use-modules (guix build utils))
                                                                      (setenv "LAMINAR_REASON" "Nightly xapian index rebuild")
                                                                      (invoke #$(file-append laminar "/bin/laminarc")
                                                                              "queue" "genenetwork3-build-xapian-index"))))
                                                #:user "laminar")))))
                   (simple-service 'install-laminar-template
                                   activation-service-type
                                   (install-laminar-template-gexp
                                    (computed-file
                                     "laminar-template.html"
                                     (laminar-template-gexp "https://issues.genenetwork.org"))))
                   (service webhook-service-type
                            (webhook-configuration
                             (socket (forge-ip-socket
                                      (ip "127.0.0.1")
                                      (port %webhook-port)))))
                   (service redis-service-type)
                   (service virtuoso-service-type
                            (virtuoso-configuration
                             (server-port 9081)
                             (http-server-port %virtuoso-sparql-port)))
                   (service genenetwork-service-type
                            (genenetwork-configuration
                             (gn2-port %genenetwork2-port)
                             (gn3-port %genenetwork3-port)
                             (genotype-files "/export/data/genenetwork/genotype_files")
                             (sparql-endpoint (string-append "http://localhost:"
                                                             (number->string %virtuoso-sparql-port)
                                                             "/sparql"))
                             (xapian-db-path %xapian-directory)))
                   (simple-service 'set-build-directory-permissions
                                   activation-service-type
                                   (with-imported-modules '((guix build utils))
                                     #~(begin
                                         (use-modules (guix build utils))

                                         (for-each (lambda (file)
                                                     (chown file
                                                            (passwd:uid (getpw "laminar"))
                                                            (passwd:gid (getpw "laminar"))))
                                                   (append (find-files #$%xapian-directory
                                                                       #:directories? #t)
                                                           (find-files #$%dump-genenetwork-database-export-directory
                                                                       #:directories? #t))))))
                   (service tissue-service-type
                            (tissue-configuration
                             (hosts
                              (list (tissue-host
                                     (name "issues.genenetwork.org")
                                     (user "laminar")
                                     (upstream-repository "https://github.com/genenetwork/gn-gemtext-threads"))))))
                   (service nginx-service-type
                            (nginx-configuration
                             (server-blocks
                              (list (development-server-reverse-proxy-server-block
                                     "9090" %genenetwork2-port %genenetwork3-port)
                                    (laminar-reverse-proxy-server-block
                                     "9090" "localhost:9089" %webhook-port
                                     (list 'gn-bioinformatics))
                                    (tissue-reverse-proxy-server-block "9090")))))
                   %base-services)))
