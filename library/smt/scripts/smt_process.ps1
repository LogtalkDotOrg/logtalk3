#############################################################################
##
##   SMT solver CLI integration script
##   Last updated on November 8, 2026
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
##
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##
##       http://www.apache.org/licenses/LICENSE-2.0
##
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
##
#############################################################################


if ($PSVersionTable.PSVersion -lt [version]'7.3') {
    [Console]::Out.WriteLine('PowerShell 7.3 or later is required')
    exit 125
}

switch ($args[0]) {
    'allocate' {
        $ErrorActionPreference = 'Continue'
        [Console]::OutputEncoding = [Text.UTF8Encoding]::new($false)
        $base = $args[1]

        for ($attempt = 0; $attempt -lt 16; $attempt++) {
            $env:LOGTALK_SMT_DIRECTORY = [IO.Path]::Combine(
                $base, 'logtalk-smt-' + [Guid]::NewGuid().ToString('N')
            )
            & $env:ComSpec /d /v:off /c 'mkdir "%LOGTALK_SMT_DIRECTORY%"' > $null 2> $null
            if ($LASTEXITCODE -eq 0) {
                [Console]::WriteLine($env:LOGTALK_SMT_DIRECTORY)
                exit 0
            }
        }
        exit 125
    }
    'run' {
        $ErrorActionPreference = 'Stop'
        $process = $null
        $output = $null
        $errors = $null
        $started = $false
        $code = 125

        try {
            $settings = [Diagnostics.ProcessStartInfo]::new()
            $settings.UseShellExecute = $false
            $settings.FileName = $args[4]
            $settings.WorkingDirectory = $args[3]
            for ($index = 5; $index -lt $args.Count; $index++) {
                $settings.ArgumentList.Add($args[$index])
            }
            $settings.RedirectStandardInput = $true
            $settings.RedirectStandardOutput = $true
            $settings.RedirectStandardError = $true

            $output = [IO.File]::Open($args[1], [IO.FileMode]::CreateNew)
            $errors = [IO.File]::Open($args[2], [IO.FileMode]::CreateNew)
            $process = [Diagnostics.Process]::new()
            $process.StartInfo = $settings
            if (-not $process.Start()) {
                throw 'spawn'
            }
            $started = $true
            $process.StandardInput.Close()

            $outputTask = $process.StandardOutput.BaseStream.CopyToAsync($output)
            $errorTask = $process.StandardError.BaseStream.CopyToAsync($errors)
            $tasks = [Threading.Tasks.Task[]]@($outputTask, $errorTask)
            while (-not [Threading.Tasks.Task]::WaitAll($tasks, 1000)) {
                if ($outputTask.IsFaulted -or $errorTask.IsFaulted -or
                    $outputTask.IsCanceled -or $errorTask.IsCanceled) {
                    throw 'capture'
                }
            }
            $process.WaitForExit()
            $code = $process.ExitCode
        }
        catch {
            if ($started -and -not $process.HasExited) {
                $process.Kill()
                $process.WaitForExit()
            }
            [Console]::Out.WriteLine('SMT solver launcher failed')
        }
        finally {
            if ($output) { $output.Dispose() }
            if ($errors) { $errors.Dispose() }
            if ($process) { $process.Dispose() }
        }
        exit $code
    }
    default {
        [Console]::Out.WriteLine('Invalid SMT solver launcher operation')
        exit 125
    }
}
