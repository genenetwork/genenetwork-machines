(use-modules (gnu)
             (gnu services mail))

(operating-system
  (host-name "mail")
  (timezone "UTC")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sdX"))))
  (file-systems %base-file-systems)
  (users %base-user-accounts)
  (packages %base-packages)

  (services (cons
             (service opensmtpd-service-type
                      (opensmtpd-configuration
                       (config-file %default-opensmtpd-config-file
                       ; (config-file (local-file "./my-smtpd.conf")))
                                    )))
             %base-services)))
