#!/usr/bin/env bash
##############################################################################################################

function priv_clippit
(
    cat <<EOF
https://google.github.io/styleguide/shellguide.html
https://guide.bash.academy
https://devhints.io/bash
https://tldr.sh

Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_lazbuild
(
    mapfile -t <"${0//sh/json}"
    declare -rA VAR=(
        [app]=$(jq --raw-output --exit-status '.app' <<< "${MAPFILE[@]}")
        [lib]=$(jq --raw-output --exit-status '.lib' <<< "${MAPFILE[@]}")
        [tst]=$(jq --raw-output --exit-status '.tst' <<< "${MAPFILE[@]}")
        [opt]=$(jq --raw-output --exit-status '.opt' <<< "${MAPFILE[@]}")
    )
    if ! [[ -d "${VAR[app]}" ]]; then
        printf '\x1b[32m\t[%s] did not find!\x1b[0m\n' "${VAR[app]}"
        exit 1
    fi >&2
    if [[ -f '.gitmodules' ]]; then
        git submodule update --init --recursive --force --remote &
    fi
    if ! (command -v lazbuild); then
        # shellcheck source=/dev/null
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus{-ide-qt5,} &
                ;;
        esac
    fi &>/dev/null
    wait
    jq --raw-output --exit-status '.pkg[]' <<< "${MAPFILE[@]}" |
        while read -r; do
            (
                declare -rA TMP=(
                    [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                    [dir]="${HOME}/.lazarus/onlinepackagemanager/packages/${REPLY}"
                    [out]=$(mktemp)
                )
                if ! [[ -d "${TMP[dir]}" ]] &&
                   ! (lazbuild --verbose-pkgsearch "${REPLY}") &&
                   ! (lazbuild --add-package "${REPLY}"); then
                        wget --quiet --output-document "${TMP[out]}" "${TMP[url]}"
                        mkdir --parents "${TMP[dir]}"
                        unzip -o "${TMP[out]}" -d "${TMP[dir]}"
                        rm --verbose "${TMP[out]}"
                        find "${TMP[dir]}" -type 'f' -name '*.lpk' -printf '\033[33m\tadd package link\t%p\033[0m\n' -exec \
                            lazbuild --add-package-link {} + >&2
                fi
            ) &
        done
    wait
    if [[ -d "${VAR[lib]}" ]]; then
        find "${VAR[lib]}" -type 'f' -name '*.lpk' |
            while read -r; do
                if ! [[ ${REPLY} =~ (cocoa|gdi|_template) ]]; then
                    lazbuild --add-package-link "${REPLY}"
                    printf '\033[33m\tadd package link\t%s\033[0m\n' "${REPLY}"
                fi
            done
    fi >&2
    declare -i exitCode=0
    if [[ -f "${VAR[tst]}" ]]; then
        read -r < <(
            lazbuild --build-all --recursive --no-write-project "${VAR[tst]}" |
                awk '/Linking/{print $3}'
        )
        if ! ("${REPLY}" --all --format=plain --progress >&2); then
            ((exitCode+=1))
        fi
    fi
    find "${VAR[app]}" -type 'f' -name '*.lpi' |
        while read -r PROJECT; do
            read -r OUTPUT < <(mktemp)
            if (lazbuild --build-all --recursive --no-write-project "${PROJECT}" > "${OUTPUT}"); then
                printf '\x1b[32m\t[%s]\t%s\x1b[0m\n' "${?}" "${PROJECT}"
                grep --color='always' 'Linking' "${OUTPUT}"
            else
                printf '\x1b[31m\t[%s]\t%s\x1b[0m\n' "${?}" "${PROJECT}"
                grep --color='always' --extended-regexp '(Error|Fatal):' "${OUTPUT}"
                ((exitCode+=1))
            fi >&2
            rm "${OUTPUT}"
        done
    exit "${exitCode}"
)

function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) priv_lazbuild ;;
            *) priv_clippit ;;
        esac
    else
        priv_clippit
    fi
)

##############################################################################################################
priv_main "${@}" >/dev/null
