function newfilename() {
    perl -e '$f=shift; $f =~ s{\.md\.[^.]+$}{.md}; print $f' \
        -- $1 $2
}

for LANG in zh_CN zh_TW ja_JP ko_KR es_ES fr_FR de_DE pt_BR en_US; do
    mkdir -p docs/$LANG
    for FILENAME in $(ls docs); do
        if [[ $FILENAME == *".lingua" ]]; then
            lingua \
                -v lang=:$LANG \
                -o docs/$LANG/$(newfilename $FILENAME) \
                "$@" \
                docs/$FILENAME
        fi
    done
done

lingua "$@" README.md.lingua
lingua "$@" crates/filter-expr/README.md.lingua
lingua "$@" crates/filter-expr-evaler/README.md.lingua
