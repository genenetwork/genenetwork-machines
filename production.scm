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
             (gnu services databases))

(operating-system
  (host-name "genenetwork")
  (timezone "UTC")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sdX"))))
  (file-systems %base-file-systems)
  (users %base-user-accounts)
  (packages %base-packages)
  (services (cons* (service mysql-service-type)
                   (service virtuoso-service-type
                            (virtuoso-configuration
                             (server-port 8891)
                             (http-server-port 8892)))
                   %base-services)))
