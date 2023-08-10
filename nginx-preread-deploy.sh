#! /bin/sh -e

nginx=$(guix build -f nginx-preread.scm)

echo $nginx
sudo ln --no-target-directory --force --symbolic $nginx/sbin/nginx /usr/local/sbin/nginx
sudo ln --force --symbolic /usr/local/sbin/nginx /var/guix/gcroots
