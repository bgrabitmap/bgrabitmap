#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    @"
vagrant  = 'it-gro/win10-ltsc-eval'
download = 'https://microsoft.com/en-us/evalcenter'
package  = 'https://learn.microsoft.com/en-us/mem/configmgr/develop/apps/how-to-create-the-windows-installer-file-msi'
shell    = 'https://learn.microsoft.com/en-us/powershell'

Usage: pwsh -File {0} [OPTIONS]
Options:
    build
    lint
"@ -f $PSCommandPath | Out-Host
}

Filter Build-Project {
    If (! (Test-Path -Path $_.app)) {
        'Did not find ' + $_.app | Out-Log
        Exit 1
    }
    If (Test-Path -Path '.gitmodules') {
        & git submodule update --init --recursive --force --remote | Out-Host
        'updated git submodule' | Out-Log
    }
    @(
        @{
            Cmd = 'lazbuild'
            Uri = 'https://fossies.org/windows/misc/lazarus-3.6-fpc-3.2.2-win64.exe'
            Path = "C:\Lazarus"
        }
    ) | Install-Program
    $_.Pkg | Install-OPM
    If (Test-Path -Path $_.lib) {
        (Get-ChildItem -Filter '*.lpk' -Recurse -File –Path $_.lib).FullName |
            Where-Object {
                $_ -notmatch '(cocoa|x11|_template)'
            } | ForEach-Object {
                & lazbuild --add-package-link $_ | Out-Null
                Return 'added {0}' -f $_
            } | Out-Log
    }
    Exit $(
        If (Test-Path -Path $_.tst) {
            $Output = (
                & lazbuild --build-all --recursive --no-write-project $_.tst |
                    Where-Object {
                        $_ -match 'Linking'
                    } | ForEach-Object {
                        $_.Split(' ')[2].Replace('bin', 'bin\.')
                    }
            )
            & $Output --all --format=plain --progress | Out-Log
            If ($LastExitCode -eq 0) { 0 } Else { 1 }
        }
    ) + (
        (Get-ChildItem -Filter '*.lpi' -Recurse -File –Path $_.app).FullName |
            ForEach-Object {
                $Output = (& lazbuild --build-all --recursive --no-write-project $_)
                $Result = @('built {0}' -f $_)
                $exitCode = If ($LastExitCode -eq 0) {
                    $Result += $Output | Where-Object { $_ -match 'Linking' }
                    0
                } Else {
                    $Result += $Output | Where-Object { $_ -match '(Error|Fatal):' }
                    1
                }
                $Result | Out-String | Out-Log
                Return $exitCode
            } | Measure-Object -Sum
    ).Sum
}

Filter Out-Log {
    $(
        If (! (Test-Path -Path Variable:LastExitCode)) {
            "$(Get-Date -uformat '%y-%m-%d_%T')$([char]27)[33m`t{0}$([char]27)[0m" -f $_
        } ElseIf ($LastExitCode -eq 0) {
            "$(Get-Date -uformat '%y-%m-%d_%T')$([char]27)[32m`t[{0}]`t{1}$([char]27)[0m" -f $LastExitCode, $_
        } Else {
            "$(Get-Date -uformat '%y-%m-%d_%T')$([char]27)[31m`t[{0}]`t{1}$([char]27)[0m" -f $LastExitCode, $_
        }
    ) | Out-Host
}

Function Install-Program {
    $Input | Where-Object {
        ! (Test-Path -Path $_.Path)
    } | ForEach-Object -Parallel {
        $_.OutFile = '{0}.{1}' -f (New-TemporaryFile).FullName, (Split-Path -Path $_.Uri -Leaf).Split('?')[0]
        Invoke-WebRequest -OutFile $_.OutFile -Uri $_.Uri
        Return $_
    } | ForEach-Object {
        If ((Split-Path -Path $_.OutFile -Leaf).Split('.')[-1] -eq 'msi') {
            & msiexec /passive /package $_.OutFile | Out-Null
        } Else {
            & $_.OutFile.Replace('Temp', 'Temp\.') /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART  | Out-Null
        }
        Remove-Item $_.OutFile
        $Env:PATH+=';{0}' -f $_.Path
        Return 'installed {0}' -f (Get-Command $_.Cmd).Source
    } | Out-Log
}

Function Install-OPM {
    $Input | ForEach-Object {
        @{
            Name = $_
            Uri = 'https://packages.lazarus-ide.org/{0}.zip' -f $_
            Path = '{0}\.lazarus\onlinepackagemanager\packages\{1}' -f $Env:APPDATA, $_
            OutFile = (New-TemporaryFile).FullName
        }
    } | Where-Object {
        ! (Test-Path -Path $_.Path) &&
        ! (& lazbuild --verbose-pkgsearch $_.Name ) &&
        ! (& lazbuild --add-package $_.Name)
    } | ForEach-Object -Parallel {
        Invoke-WebRequest -OutFile $_.OutFile -Uri $_.Uri
        New-Item -Type Directory -Path $_.Path | Out-Null
        Expand-Archive -Path $_.OutFile -DestinationPath $_.Path
        Remove-Item $_.OutFile
        Return (Get-ChildItem -Filter '*.lpk' -Recurse -File –Path $_.Path).FullName
    } | ForEach-Object {
        & lazbuild --add-package-link $_ | Out-Null
        Return 'added {0}' -f $_
    } | Out-Log
}

Filter Ping-Connect {
    @{
        Method = 'POST'
        Headers = @{
            ContentType = 'application/json'
        }
        Uri = 'https://postman-echo.com/post'
        Body = $_ | ConvertTo-Json
    } | Invoke-WebRequest | ConvertFrom-Json | Out-Host
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict #-Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        If ($args[0] -eq 'build') {
            Get-Content -Path $PSCommandPath.Replace('ps1', 'json') |
                ConvertFrom-Json |
                Build-Project
        } Else {
            Invoke-ScriptAnalyzer -EnableExit -Recurse -Path '.'
            (Get-ChildItem -Filter '*.ps1' -Recurse -Path '.').FullName |
                ForEach-Object {
                    Invoke-Formatter -ScriptDefinition $(
                        Get-Content -Path $_ | Out-String
                    ) | Set-Content -Path $_
                }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args
