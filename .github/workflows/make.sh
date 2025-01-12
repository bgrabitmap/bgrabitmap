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
    mapfile -t <"${0//sh/json}"
    declare -A VAR
    while read -r; do
        VAR[${REPLY}]=$(jq --raw-output --exit-status ".${REPLY}" <<< "${MAPFILE[@]}")
    done < <(jq --raw-output --exit-status 'keys.[]' <<< "${MAPFILE[@]}")
    declare -rA VAR
    if ! [[ -d "${VAR[app]}" ]]; then
        out_log error "Did not find ${VAR[app]}"
    fi >&2
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
        ) & done < <(jq --raw-output --exit-status '.pkg[]' <<< "${MAPFILE[@]}")
        wait
    )
    if [[ -d ${VAR[lib]} ]]; then
        while read -r; do
            if ! [[ ${REPLY} =~ (cocoa|gdi|_template) ]]; then
                lazbuild --add-package-link "${REPLY}"
                out_log audit "added ${REPLY}"
            fi
        done < <(find "${VAR[lib]}" -type 'f' -name '*.lpk')
    fi >&2
    declare -i exitCode=0
    if [[ -f ${VAR[tst]} ]]; then
        read -r < <(
            lazbuild --build-all --recursive --no-write-project "${VAR[tst]}" |
                awk '/Linking/{print $3}'
        )
        if ! ("${REPLY}" --all --format=plain --progress >&2); then
            ((exitCode+=1))
        fi
    fi
    while read -r; do
        mapfile -t < <(mktemp)
        if (lazbuild --build-all --recursive --no-write-project "${REPLY:?}" > "${MAPFILE[0]:?}"); then
            out_log info "built ${REPLY:?}"
            grep --color='always' 'Linking' "${MAPFILE[0]:?}"
        else
            out_log audit "built ${REPLY:?}"
            grep --color='always' --extended-regexp '(Error|Fatal):' "${MAPFILE[0]:?}"
            ((exitCode+=1))
        fi >&2
        rm "${MAPFILE[0]:?}"
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
    printf '%(%y-%m-%d_%T)T\x1b[%dm\t%s:\t%b\x1b[0m\n' -1 "${VAR[${1,,:?}]}" "${1^^}" "${2:?}" >&2
    case ${1:?} in
        error) return 1 ;;
    esac
)

function switch_action
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) build_project ;;
            *) show_usage ;;
        esac
    else
        show_usage
    fi
)

##############################################################################################################
switch_action "${@}" >/dev/null
