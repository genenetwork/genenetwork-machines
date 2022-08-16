;;; genenetwork-machines --- Guix configuration for genenetwork machines
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
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
             (gnu services shepherd)
             (gnu services web)
             (guix build-system gnu)
             (guix channels)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix modules)
             (guix packages)
             (guix profiles)
             (guix records)
             (guix store)
             (forge forge)
             (forge laminar)
             (forge tissue)
             (forge utils)
             (forge webhook)
             (srfi srfi-1)
             (ice-9 match))

;; guix-daemon socket of the host shared inside the container
(define %guix-daemon-uri
  "/var/host-guix/daemon-socket/socket")

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


;;;
;;; Development server configuration
;;;

;; A development server is a server that is automatically redeployed
;; on every commit to its git repository. This is used by both the
;; genenetwork2 and the genenetwork3 development servers. Here, we
;; abstract out the details.

(define-record-type* <development-server-configuration>
  development-server-configuration make-development-server-configuration
  development-server-configuration?
  (name development-server-configuration-name)
  (git-repository development-server-configuration-git-repository)
  (git-branch development-server-configuration-git-branch)
  (executable-path development-server-configuration-executable-path)
  (runner development-server-configuration-runner)
  (port development-server-configuration-port
        (default 8080)))

(define (development-server-redeploy config)
  "Return a G-expression that provides a function redeploy which
redeploys the development server specified by CONFIG, a
<development-server-configuration> object."
  (with-imported-modules (source-module-closure '((genenetwork development-helper)
                                                  (forge build git))
                                                #:select? import-module?)
    (with-extensions (list guile-gcrypt guile-zlib)
      (with-packages (list git-minimal nss-certs)
        #~(begin
            (use-modules (genenetwork development-helper)
                         (forge build git)
                         (guix derivations)
                         (guix gexp)
                         (guix monads)
                         (guix store)
                         (guix utils)
                         (rnrs exceptions))
            
            (define (redeploy)
              (parameterize ((%daemon-socket-uri #$%guix-daemon-uri))
                (switch-symlinks
                 #$(development-server-configuration-executable-path config)
                 (with-store store
                   (run-with-store store
                     (mlet* %store-monad ((latest-source
                                           (latest-git-checkout
                                            #$(string-append (development-server-configuration-name config)
                                                             "-checkout")
                                            #$(development-server-configuration-git-repository config)
                                            #:branch #$(development-server-configuration-git-branch config)))
                                          (runner-drv
                                           (gexp->script
                                            #$(string-append (development-server-configuration-name config)
                                                             "-runner")
                                            (#$(development-server-configuration-runner config)
                                             latest-source)
                                            #:guile (read-derivation-from-file
                                                     #$(raw-derivation-file
                                                        (with-store store
                                                          (package-derivation store guile-3.0)))))))
                       (mbegin %store-monad
                         (built-derivations (list runner-drv))
                         (return (derivation->output-path runner-drv))))))))))))))

(define (development-server-activation config)
  "Return a G-expression that runs the development server specified by
CONFIG, a <development-server-configuration> object, on startup."
  (let ((executable-path (development-server-configuration-executable-path config)))
    #~(begin
        (use-modules (guix build utils))
      
        (mkdir-p #$(dirname executable-path))
        (chown #$(dirname executable-path)
               (passwd:uid (getpw "laminar"))
               (passwd:gid (getpw "laminar")))
        #$(development-server-redeploy config)
        (redeploy))))


;;;
;;; genenetwork2
;;;

;; Path to genotype files required by genenetwork2
(define %genotype-files
  "/export/data/genenetwork/genotype_files")

;; Port on which genenetwork3 is listening
(define %genenetwork3-port
  9093)

(define (genenetwork2-tests project test-command)
  "Return a G-expression that runs TEST-COMMAND with genenetwork2
source code. PROJECT is a <forge-project> object describing
genenetwork2. TEST-COMMAND is a list of strings specifying the command
to be executed."
  (with-imported-modules '((guix build utils))
    (with-packages (list bash coreutils git-minimal nss-certs)
      #~(begin
          (use-modules (guix build utils))

          (define (hline)
            "Print a horizontal line 50 '=' characters long."
            (display (make-string 50 #\=))
            (newline)
            (force-output))
          
          (invoke "git" "clone" "--depth" "1"
                  "--branch" #$(forge-project-repository-branch project)
                  #$(forge-project-repository project)
                  ".")
          (hline)
          (invoke "git" "log" "--max-count" "1")
          (hline)
          (setenv "SERVER_PORT" "8080")
          (setenv "GN2_PROFILE"
                  #$(profile
                     (content (package->development-manifest genenetwork2))
                     (allow-collisions? #t)))
          (setenv "GN_PROXY_URL" "http://genenetwork.org/gn3-proxy/")
          (setenv "GN3_LOCAL_URL" "http://localhost:9093")
          (setenv "GENENETWORK_FILES" #$%genotype-files)
          (setenv "HOME" "/tmp")
          (setenv "SQL_URI" "mysql://webqtlout:webqtlout@localhost/db_webqtl")
          (apply invoke '#$test-command)))))

(define (genenetwork2-project config)
  "Return a forge project describing genenetwork2. CONFIG is a
<development-server-configuration> object describing genenetwork2."
  (forge-project
   (name "genenetwork2")
   (repository (development-server-configuration-git-repository config))
   (repository-branch (development-server-configuration-git-branch config))
   (ci-jobs (list (forge-laminar-job
                   (name "genenetwork2")
                   (run (genenetwork2-tests
                         this-forge-project
                         (list "sh" "bin/genenetwork2" "./etc/default_settings.py"
                               "-c" "-m" "unittest" "-v")))
                   ;; If unit tests pass, redeploy genenetwork2.
                   (after #~(begin
                              (use-modules (guix build utils))
                              #$(development-server-redeploy config)
                              (when (string=? (getenv "RESULT") "success")
                                (redeploy)
                                ;; We cannot refer to sudo in the
                                ;; store since that sudo does not have
                                ;; the setuid bit set. See "(guix)
                                ;; Setuid Programs".
                                (invoke "/run/setuid-programs/sudo"
                                        #$(file-append shepherd "/bin/herd")
                                        "restart" "genenetwork2")
                                (invoke #$(file-append laminar "/bin/laminarc")
                                        "queue" "genenetwork2-mechanical-rob")))))
                  (forge-laminar-job
                   (name "genenetwork2-mechanical-rob")
                   (run (genenetwork2-tests
                         this-forge-project
                         (list "sh" "bin/genenetwork2" "./etc/default_settings.py"
                               "-c" "../test/requests/test-website.py"
                               "--all" "http://localhost:9092")))
                   (trigger? #f))))
   (ci-jobs-trigger 'webhook)))

(define (genenetwork2-shepherd-service config)
  "Return a shepherd service to run the genenetwork2 development
server. CONFIG is a <development-server-configuration> object
describing genenetwork2."
  (shepherd-service
   (documentation "Run GeneNetwork 2 development server.")
   (provision '(genenetwork2))
   ;; FIXME: The genenetwork2 service should depend on redis.
   (requirement '(networking genenetwork3))
   (modules '((gnu build shepherd)
              (gnu system file-systems)))
   (start
    (with-imported-modules (source-module-closure '((gnu build shepherd)
                                                    (gnu system file-systems)))
      #~(make-forkexec-constructor/container
         (list #$(development-server-configuration-executable-path config)
               "127.0.0.1" (number->string #$(development-server-configuration-port config)))
         #:mappings (list (file-system-mapping
                           (source #$(development-server-configuration-executable-path config))
                           (target source))
                          (file-system-mapping
                           (source #$%genotype-files)
                           (target source))
                          (file-system-mapping
                           (source "/run/mysqld/mysqld.sock")
                           (target source)
                           (writable? #t)))
         #:log-file "/var/log/genenetwork2.log")))
   (stop #~(make-kill-destructor))))

(define %default-genenetwork2-configuration
  (development-server-configuration
   (name "genenetwork2")
   (git-repository "https://github.com/genenetwork/genenetwork2")
   (git-branch "testing")
   (executable-path "/laminar/bin/genenetwork2")
   (runner (with-imported-modules (source-module-closure '((genenetwork development-helper))
                                                         #:select? import-module?)
             #~(lambda (latest-gn2-source)
                 ((@@ (genenetwork development-helper) genenetwork2-runner-gexp)
                  latest-gn2-source
                  #$(profile
                     (content (package->development-manifest genenetwork2))
                     (allow-collisions? #t))
                  #$%genenetwork3-port
                  #$%genotype-files))))))

(define genenetwork2-service-type
  (service-type
   (name 'genenetwork2)
   (description "Run GeneNetwork 2 development server and CI.")
   (extensions
    (list (service-extension activation-service-type
                             development-server-activation)
          (service-extension shepherd-root-service-type
                             (compose list genenetwork2-shepherd-service))
          (service-extension forge-service-type
                             (compose list genenetwork2-project))))
   (default-value %default-genenetwork2-configuration)))


;;;
;;; genenetwork3
;;;

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
  (genenetwork3-tests (list "python3" "-m" "unittest")
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

(define (genenetwork3-project config)
  (forge-project
   (name "genenetwork3")
   (repository (development-server-configuration-git-repository config))
   (repository-branch (development-server-configuration-git-branch config))
   (ci-jobs (list (forge-laminar-job
                   (name "genenetwork3")
                   (run (derivation-job-gexp
                         this-forge-project
                         this-forge-laminar-job
                         genenetwork3-unit-tests
                         #:guix-daemon-uri %guix-daemon-uri))
                   ;; If unit tests pass, redeploy genenetwork3.
                   (after #~(begin
                              (use-modules (guix build utils))
                              #$(development-server-redeploy config)
                              (when (string=? (getenv "RESULT") "success")
                                (redeploy)
                                ;; We cannot refer to sudo in the
                                ;; store since that sudo does not have
                                ;; the setuid bit set. See "(guix)
                                ;; Setuid Programs".
                                (invoke "/run/setuid-programs/sudo"
                                        #$(file-append shepherd "/bin/herd")
                                        "restart" "genenetwork3")))))
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
                         #:guix-daemon-uri %guix-daemon-uri)))))
   (ci-jobs-trigger 'webhook)))

(define (genenetwork3-shepherd-service config)
  (shepherd-service
   (documentation "Run GeneNetwork 3.")
   (provision '(genenetwork3))
   (requirement '(networking))
   (modules '((gnu build shepherd)
              (gnu system file-systems)))
   (start
    (with-imported-modules (source-module-closure '((gnu build shepherd)
                                                    (gnu system file-systems)))
      #~(make-forkexec-constructor/container
         (list #$(development-server-configuration-executable-path config)
               "127.0.0.1" #$(number->string (development-server-configuration-port config)))
         #:mappings (list (file-system-mapping
                           (source #$(development-server-configuration-executable-path config))
                           (target source))
                          (file-system-mapping
                           (source "/run/mysqld/mysqld.sock")
                           (target source)
                           (writable? #t)))
         #:log-file "/var/log/genenetwork3.log")))
   (stop #~(make-kill-destructor))))

(define %default-genenetwork3-configuration
  (development-server-configuration
   (name "genenetwork3")
   (git-repository "https://github.com/genenetwork/genenetwork3")
   (git-branch "main")
   (executable-path "/laminar/bin/genenetwork3")
   (runner (with-imported-modules (source-module-closure '((genenetwork development-helper))
                                                         #:select? import-module?)
             #~(lambda (latest-gn3-source)
                 ((@@ (genenetwork development-helper) genenetwork3-runner-gexp)
                  latest-gn3-source
                  #$(profile
                     (content (manifest-cons gunicorn
                                             (package->development-manifest genenetwork3)))
                     (allow-collisions? #t))))))))

(define genenetwork3-service-type
  (service-type
   (name 'genenetwork3)
   (description "Run GeneNetwork 3.")
   (extensions
    (list (service-extension activation-service-type
                             development-server-activation)
          (service-extension shepherd-root-service-type
                             (compose list genenetwork3-shepherd-service))
          (service-extension forge-service-type
                             (compose list genenetwork3-project))))
   (default-value %default-genenetwork3-configuration)))


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
                         nss-certs virtuoso-ose)
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
   (repository "https://git.genenetwork.org/arunisaac/dump-genenetwork-database")
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
   (website-directory "/srv/http/issues")
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

        (set-path-environment-variable "PATH"
                                       (list "bin")
                                       (list #$gzip #$tar))
        (invoke "tar" "--extract" "--strip-components=3"
                (string-append "--file=" #$(package-source laminar))
                (string-append "laminar-"
                               #$(package-version laminar)
                               "/src/resources/index.html"))
        (copy-file "index.html" #$output)
        (substitute* #$output
          (("<router-link to=\"jobs\">Jobs</router-link>" jobs-link)
           (string-append
            "<a href=\"/channels.scm\" target=\"_blank\">channels.scm</a>"
            jobs-link
            "<a href=\"" #$issue-tracker-uri "\" target=\"_blank\">Issues</a>"))))))

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

(define (laminar-reverse-proxy-server-block listen laminar-bind-http published-channel-names)
  "Return an <nginx-server-configuration> object to reverse proxy
laminar. The nginx server will listen on LISTEN and reverse proxy to
laminar listening on LAMINAR-BIND-HTTP. PUBLISHED-CHANNEL-NAMES is a
list of channel names for which a channels.scm should be published."
  (nginx-server-configuration
   (server-name '("localhost"))
   (listen (list listen))
   (locations
    (list (nginx-location-configuration
           (uri "/")
           (body (list (string-append "proxy_pass http://" laminar-bind-http ";")
                       ;; Disable proxy buffering in host's nginx. We
                       ;; need this to allow Laminar's Server-Sent
                       ;; Events to pass through.
                       "proxy_pass_header X-Accel-Buffering;")))
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
                   (simple-service 'install-laminar-template
                                   activation-service-type
                                   (install-laminar-template-gexp
                                    (computed-file
                                     "laminar-template.html"
                                     (laminar-template-gexp "https://issues.genenetwork.org"))))
                   (service webhook-service-type
                            (webhook-configuration
                             (port 9091)))
                   (service redis-service-type)
                   (service virtuoso-service-type
                            (virtuoso-configuration
                             (server-port 8891)
                             (http-server-port 8892)))
                   (service genenetwork2-service-type
                            (development-server-configuration
                             (inherit %default-genenetwork2-configuration)
                             (port 9092)))
                   (service genenetwork3-service-type
                            (development-server-configuration
                             (inherit %default-genenetwork3-configuration)
                             (port %genenetwork3-port)))
                   (simple-service 'set-dump-genenetwork-database-export-directory-permissions
                                   activation-service-type
                                   (with-imported-modules '((guix build utils))
                                     #~(begin
                                         (use-modules (guix build utils))

                                         (for-each (lambda (file)
                                                     (chown file
                                                            (passwd:uid (getpw "laminar"))
                                                            (passwd:gid (getpw "laminar"))))
                                                   (find-files #$%dump-genenetwork-database-export-directory
                                                               #:directories? #t)))))
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
                              (list (laminar-reverse-proxy-server-block
                                     "9090" "localhost:9089"
                                     (list 'gn-bioinformatics 'guix))
                                    (tissue-reverse-proxy-server-block "9090")))))
                   %base-services)))
