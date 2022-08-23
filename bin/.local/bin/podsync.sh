#! /bin/bash

cd /home/e/media/mus/podcasts

# Ref: https://unix.stackexchange.com/a/389107
#find . -type f -name '*:*' -execdir bash -c 'mv "$1" "${1//:/-}"' bash {} \;
# Ref: https://stackoverflow.com/a/44914712
for file in *; do mv "$file" $(echo "$file" | sed -e 's/[^A-Za-z0-9._-]/_/g'); done &

# Ref: https://stackoverflow.com/a/60687513
export n=50 # change according to your needs
find . -type f                      \
     ! -name '.*'                   \
       -regextype egrep             \
     ! -regex '.*\.[^/.]{'"$n"',}'  \
       -regex '.*[^/]{'$((n+1))',}' \
       -execdir bash -c '
    for f in "${@#./}"; do
        ext=${f#"${f%.*}"}
        mv -- "$f" "${f:0:n-${#ext}}${ext}"
    done' bash {} +

rsync -av "/home/e/media/mus/podcasts/" "/media/e/SPORT PLUS/Podcasts/" --remove-source-files

notify-send "Podcast sync complete" -i /usr/share/icons/retrosmart-icon-theme/scalable/cloud.svg
