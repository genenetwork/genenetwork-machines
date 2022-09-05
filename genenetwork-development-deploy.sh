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

container_script=$(guix system container --network \
                        --verbosity=3 \
                        --load-path=. \
                        --share=/var/guix/daemon-socket=/var/host-guix/daemon-socket \
                        --share=/var/guix-containers/genenetwork-development/var/lib/laminar=/var/lib/laminar \
                        --share=/var/guix-containers/genenetwork-development/var/lib/tissue=/var/lib/tissue \
                        --share=/var/guix-containers/genenetwork-development/var/lib/virtuoso=/var/lib/virtuoso \
                        --share=/var/guix-containers/genenetwork-development/var/log/cd=/var/log/cd \
                        --expose=/export/data/genenetwork/genotype_files \
                        --share=/export/genenetwork-database-dump \
                        --share=/var/run/mysqld/mysqld.sock=/run/mysqld/mysqld.sock \
                        genenetwork-development.scm)

echo $container_script
sudo ln --force --symbolic $container_script /usr/local/bin/genenetwork-development-container
