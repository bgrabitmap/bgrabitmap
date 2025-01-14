#!/usr/bin/env bash
##############################################################################################################

function show_usage
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

function build_project
(
    mapfile -t
    declare -A VAR
    while read -r; do
        VAR[${REPLY}]=$(jq --raw-output --exit-status ".${REPLY}" <<<"${MAPFILE[@]}")
    done < <(jq --raw-output --exit-status 'keys.[]' <<<"${MAPFILE[@]}")
    declare -rA VAR
    if [[ -f '.gitmodules' ]]; then
        git submodule update --init --recursive --force --remote
        out_log audit 'updated git submodule'
    fi &
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
    while read -r; do
        lazbuild --add-package-link "${REPLY}"
        out_log audit "added ${REPLY}"
    done < <(
        while read -r; do (
            declare -rA TMP=(
                [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                [dir]="${HOME}/.lazarus/onlinepackagemanager/packages/${REPLY}"
                [out]=$(mktemp)
            )
            if ! [[ -d ${TMP[dir]} ]] &&
                ! (lazbuild --verbose-pkgsearch "${REPLY}" >/dev/null) &&
                ! (lazbuild --add-package "${REPLY}" >/dev/null); then
                (
                    wget --quiet --output-document "${TMP[out]}" "${TMP[url]}"
                    mkdir --parents "${TMP[dir]}"
                    unzip -o "${TMP[out]}" -d "${TMP[dir]}"
                    rm "${TMP[out]:?}"
                ) >/dev/null
                find "${TMP[dir]}" -type 'f' -name '*.lpk'
            fi
        ) & done < <(jq --raw-output --exit-status '.pkg[]' <<<"${MAPFILE[@]}")
        wait
    )
    while read -r; do
        if ! [[ ${REPLY} =~ (cocoa|gdi|_template) ]]; then
            lazbuild --add-package-link "${REPLY}"
            out_log audit "added ${REPLY}"
        fi >&2
    done < <(find "${VAR[lib]}" -type 'f' -name '*.lpk')
    declare -i exitCode=0
    if [[ -f ${VAR[tst]} ]]; then
        while read -r; do
            if [[ ${REPLY} =~ Linking ]]; then
                "${REPLY##* }" --all --format=plain --progress >&2 ||
                    exitCode+=1
            fi
        done < <(lazbuild --build-all --recursive --no-write-project "${VAR[tst]}")
    fi
    while read -r; do
        if (lazbuild --build-all --recursive --no-write-project "${REPLY:?}"); then
            out_log info "built ${REPLY:?}"
        else
            exitCode+=1
            out_log audit "built ${REPLY:?}"
        fi | grep --color='always' --extended-regexp '(Error:|Fatal:|Linking)' >&2
    done < <(find "${VAR[app]}" -type 'f' -name '*.lpi')
    exit "${exitCode}"
)

function out_log
(
    declare -Ar VAR=(
        [error]=31
        [info]=32
        [audit]=33
    )
    printf '%(%y-%m-%d_%T)T \x1b[%dm%b\x1b[0m\n' -1 "${VAR[${1,,:?}]}" "${2:?}" >&2
    case ${1:?} in
        error) return 1 ;;
    esac
)

function switch_action
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) build_project <"${0//sh/json}" ;;
            *) show_usage ;;
        esac
    else
        show_usage
    fi
)

##############################################################################################################
switch_action "${@}" >/dev/null
