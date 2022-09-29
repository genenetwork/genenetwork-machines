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

(define-module (genenetwork development-helper)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (forge build utils))

(define (command-in-source-gexp source profile command)
  "Return a G-expression that runs COMMAND in PROFILE and with SOURCE
as the current directory. SOURCE and PROFILE are store items. COMMAND
is a list of strings specifying the command to be executed."
  (with-imported-modules '((guix build utils))
    (with-profile profile
      #~(begin
          (use-modules (rnrs exceptions)
                       (guix build utils))

          (chdir #$source)
          (guard (condition ((invoke-error? condition)
                             (format (current-error-port)
                                     "`~a~{ ~a~}' failed with exit status ~a~%"
                                     (invoke-error-program condition)
                                     (invoke-error-arguments condition)
                                     (invoke-error-exit-status condition))
                             (exit #f)))
            (apply invoke '#$command))
          (mkdir-p #$output)))))

(define (asdf-test-gexp source profile system asd-files)
  "Return a G-expression that tests SYSTEM using asdf after loading
ASD-FILES. SOURCE is the source code under test. PROFILE is a profile
with all necessary dependencies. SOURCE and PROFILE are both store
items."
  (with-imported-modules '((guix build lisp-utils)
                           (guix build utils))
    (with-profile profile
      #~(begin
          (use-modules (guix build lisp-utils)
                       (guix build utils))

          (chdir #$source)
          (setenv "HOME" "/tmp")
          (parameterize ((%lisp "sbcl")
                         (%lisp-type "sbcl"))
            (test-system #$system '#$asd-files #f))
          (mkdir-p #$output)))))

(define (genenetwork3-lint-gexp genenetwork3-source profile)
  "Return a G-expression that runs GeneNetwork3 lint tests in PROFILE
with GENENETWORK3-SOURCE as the current directory. GENENETWORK3-SOURCE
is a checkout of the genenetwork3 source code. PROFILE is a profile
with genenetwork3 dependencies."
  (with-imported-modules '((guix build utils))
    (with-profile profile
      #~(begin
          (use-modules (rnrs exceptions)
                       (srfi srfi-26)
                       (ice-9 rdelim)
                       (guix build utils))

          (define (shell-script? filename stat-obj)
            (and (eq? (stat:type stat-obj) 'regular)
                 (call-with-input-file filename
                   (lambda (port)
                     (let ((first-line (read-line port)))
                       (and (not (eof-object? first-line))
                            (> (string-length first-line) 2)
                            (string=? (string-take first-line 2) "#!")
                            (or (string-contains first-line "/bin/sh")
                                (string-contains first-line "/bin/bash"))))))))
          
          (chdir #$genenetwork3-source)
          (guard (condition ((invoke-error? condition)
                             (format (current-error-port)
                                     "`~a~{ ~a~}' failed with exit status ~a~%"
                                     (invoke-error-program condition)
                                     (invoke-error-arguments condition)
                                     (invoke-error-exit-status condition))
                             (exit #f)))
            (for-each (cut invoke "shellcheck" <>)
                      (find-files "." shell-script?))
            (invoke "pylint" "gn3"))
          (mkdir-p #$output)))))

(define (genenetwork2-runner-gexp genenetwork2-source profile gn3-port
                                  genotype-files xapian-db-path)
  "Return a G-expression that runs the genenetwork2 server for
GENENETWORK2-SOURCE in PROFILE. GENENETWORK2-SOURCE is a checkout of
the genenetwork2 source code. PROFILE is a profile with genenetwork2
dependencies. GN3-PORT is the port on which a local instance of
genenetwork3 is listening. GENOTYPE-FILES is the path to genotype
files. XAPIAN-DB-PATH is the path to the xapian search index."
  (with-imported-modules '((guix build utils))
    (with-profile profile
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))
          
          (chdir #$genenetwork2-source)
          (match (command-line)
            ((_ ip port)
             (setenv "SERVER_PORT" port)
             (setenv "GN2_PROFILE" #$profile)
             (setenv "GN_PROXY_URL" "http://genenetwork.org/gn3-proxy/")
             (setenv "GN_SERVER_URL" "/api3")
             (setenv "GN3_LOCAL_URL"
                     (string-append "http://localhost:"
                                    (number->string #$gn3-port)))
             (setenv "GENENETWORK_FILES" #$genotype-files)
             (setenv "SQL_URI" "mysql://webqtlout:webqtlout@localhost/db_webqtl")
             (setenv "HOME" "/tmp")
             (setenv "NO_REDIS" "no-redis")
             (setenv "XAPIAN_DB_PATH" #$xapian-db-path)
             (invoke "sh" "bin/genenetwork2" "etc/default_settings.py" "-gunicorn-prod")))))))

(define (genenetwork3-runner-gexp genenetwork3-source profile)
  "Return a G-expression that runs the genenetwork3 server for
GENENETWORK3-SOURCE in PROFILE. GENENETWORK3-SOURCE is a checkout of
the genenetwork3 source code. PROFILE is a profile with genenetwork3
dependencies."
  (with-imported-modules '((guix build utils))
    (with-profile profile
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))
          
          (chdir #$genenetwork3-source)
          (match (command-line)
            ((_ ip port)
             (invoke "gunicorn"
                     "-b" (string-append ip ":" port)
                     "gn3.app:create_app()")))))))
