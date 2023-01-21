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
            (invoke "pylint" "*py" "tests" "gn3" "scripts" "sheepdog"))
          (mkdir-p #$output)))))
