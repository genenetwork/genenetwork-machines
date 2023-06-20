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
             (gn services databases)
             (gnu services web))

(define (virtuoso-reverse-proxy-server-block listen sparql-port)
  "Return an <nginx-server-configuration> object listening on LISTEN to
reverse proxy the Virtuoso server. SPARQL-PORT is the port virtuoso's
SPARQL endpoint is listening on."
  (nginx-server-configuration
   (server-name '("sparql.genenetwork.org"))
   (listen (list listen))
   (locations
    (list (nginx-location-configuration
           (uri "/")
           (body (list (string-append "proxy_pass http://localhost:"
                                      (number->string sparql-port) ";")
                       "proxy_set_header Host $host;")))))))

(define %reverse-proxy-port 8990)
(define %virtuoso-port 8981)
(define %sparql-port 8982)

(operating-system
  (host-name "sparql")
  (timezone "UTC")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sdX"))))
  (file-systems %base-file-systems)
  (users %base-user-accounts)
  (packages %base-packages)
  (services (cons* (service virtuoso-service-type
                            (virtuoso-configuration
                             (server-port %virtuoso-port)
                             (http-server-port %sparql-port)
			     (number-of-buffers 4000000)
			     (dirs-allowed "/var/lib/data")
			     (maximum-dirty-buffers 3000000)))
                   (service nginx-service-type
                            (nginx-configuration
                             (server-blocks
                              (list (virtuoso-reverse-proxy-server-block
                                     (number->string %reverse-proxy-port)
                                     %sparql-port)))))
                   %base-services)))
