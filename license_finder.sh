#!/bin/bash
report_dir="$(mktemp -d /tmp/crowbar-license-report-XXXXXX)" || exit 1

handle_one_package() {
    local pkg line files
    pkg="${1##*/}"
    [[ -d $report_dir/$pkg ]] && continue
    files=()
    case $1 in
        *.rpm)
            # RPM files know what licenses govern the contents of a given RPM.
            # Stash that information in a handy spreadsheet
            rpm --qf "${pkg},%{NAME},%{VERSION},%{LICENSE},%{URL}\n" -qp "$1" 2>/dev/null >> "$report_dir/crowbar-rpm-licenses.csv"
            # Now, look for anything that might be a license file in the RPM and extract it.
            read -a files < <(rpm -qlp "$1" 2>/dev/null |grep -i -e license -e licence -e copyright)
            [[ $files ]] || continue
            files=("${files[@]#/}")
            mkdir -p "$report_dir/$pkg"
            rpm2cpio "$1" >"$report_dir/$pkg/$pkg.cpio" 2>/dev/null
            echo "Extracting ${files[*]} from $pkg"
            (   cd "$report_dir/$pkg"
                cpio -id <"$pkg.cpio"
                mv "${files[@]}" .
                find . -type d -mindepth 1 -depth -exec rm -rf '{}' ';'
            )
            rm "$report_dir/$pkg/$pkg.cpio"
           ;;
        *.deb)
            # Sadly, most .deb files have no embedded license information.
            # However, they pretty much all will install their license in /usr/share/doc somewhere.
            # So, we can look for that and grab it.
            read -a files <(dpkg-deb -c "$1" |grep -i -e license -e licence -e copyright)
            [[ $files ]] || continue
            mkdir -p "$report_dir/$pkg"
            dpkg-deb --fsys-tarfile "$1" > "$report_dir/$pkg/$pkg.tar"
            read -a files < <(tar --force-local -tf "$report_dir/$pkg/$pkg.tar" |grep -i -e license -e licence -e copyright)
            echo "Extracting ${files[*]} from $pkg"
            (   cd "$report_dir/$pkg"
                tar xf "$pkg.tar" "${files[@]}"
                mv "${files[@]}" .
                find . -type d -mindepth 1 -depth -exec rm -rf '{}' ';'
            )
            rm "$report_dir/$pkg/$pkg.tar"
            ;;
    esac
}

handle_packages_in() {
    local l
    while read l; do
        handle_one_package "$l"
    done < <(command find "$1" -name '*.deb' -or -name '*.rpm')
}

handle_barclamp() {
    # $1 = full path to barclamp tarball
    local dest="${1##*/}" f
    dest="${dest%.tar*}"
    mkdir -p "$report_dir/$dest"
    tar -xf "$1" -C "$report_dir/$dest"
    handle_packages_in "$report_dir/$dest"
    rm -rf "$report_dir/$dest"
}

handle_iso() {
    local f
    mkdir -p "$report_dir/iso"
    sudo mount -o loop "$1" "$report_dir/iso"
    handle_packages_in "$report_dir/iso"
    for f in "$report_dir/iso/dell/barclamps/"*.tar*; do
        handle_barclamp "$f"
    done
    sudo umount "$report_dir/iso"
}

handle_iso "$1"
echo "License report is in $report_dir"
