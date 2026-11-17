function newfilename() {
    perl -e '$f=shift; $l=shift; $f =~ s{\.md\.[^.]+$}{.$l.md}; print $f' \
        -- $1 $2
}

for LANG in zh_CN zh_TW ja_JP ko_KR es_ES fr_FR de_DE pt_BR en_US; do
    for FILENAME in $(ls docs); do
        if [[ $FILENAME == *".lingua" ]]; then
            lingua \
                -v lang=:$LANG \
                -o docs/$(newfilename $FILENAME $LANG) \
                "$@" \
                docs/$FILENAME
        fi
    done
done

lingua "$@" README.md.lingua
