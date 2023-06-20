#! /bin/sh -e

# genenetwork-machines --- Guix configuration for genenetwork machines
# Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of genenetwork-machines.
#
# genenetwork-machines is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# genenetwork-machines is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with genenetwork-machines.  If not, see
# <https://www.gnu.org/licenses/>.

# Build and install genenetwork development container on penguin2.

# If we shared only the mysqld.sock socket file, it would break when
# the external mysqld server is restarted. So, we share the mysqld
# socket directory.
container_script=$(guix system container --network \
                        --verbosity=3 \
                        --load-path=. \
                        --share=/var/guix/daemon-socket=/var/host-guix/daemon-socket \
                        --share=/export2/guix-containers/genenetwork-development/var/lib/laminar=/var/lib/laminar \
                        --share=/export2/guix-containers/genenetwork-development/var/lib/tissue=/var/lib/tissue \
                        --share=/export2/guix-containers/genenetwork-development/var/lib/virtuoso=/var/lib/virtuoso \
                        --share=/export2/guix-containers/genenetwork-development/var/log/cd=/var/log/cd \
			--share=/export/data/genenetwork-virtuoso=/var/lib/data \
                        --expose=/export/data/genenetwork \
                        --share=/export/data/genenetwork-xapian \
                        --share=/export/data/genenetwork-sqlite \
                        --share=/export/genenetwork-database-dump \
                        --share=/var/run/mysqld=/run/mysqld \
                        genenetwork-development.scm)

echo $container_script
sudo ln --force --symbolic $container_script /usr/local/bin/genenetwork-development-container
sudo ln --force --symbolic /usr/local/bin/genenetwork-development-container /var/guix/gcroots
