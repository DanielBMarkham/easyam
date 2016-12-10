Write-Host "setup.ps1 start"

try {
	Remove-Item  ../../../test/in/easyam.exe
	Remove-Item  ../../../test/in/easyam.exe.config
	Remove-Item ../../../test/in/easyam.xml
	Remove-Item  ../../../test/in/easyam.pdb
	Remove-Item  ../../../test/in/FSharp.Core.dll
	Remove-Item ../../../test/in/FSharp.Core.xml
	Remove-Item ../../../test/in/FSharp.Data.TypeProviders.dll
}
catch {

}

if ((Test-Path ../../../test/in/Behavior)) {Remove-Item ../../../test/in/Behavior -Force -Recurse}
if ((Test-Path ../../../test/in/Structure)) {Remove-Item ../../../test/in/Structure -Force -Recurse}
if ((Test-Path ../../../test/in/Supplemental)) {Remove-Item ../../../test/in/Supplemental -Force -Recurse}
if ((Test-Path ../../../test/in/Meta)) {Remove-Item ../../../test/in/Meta -Force -Recurse}

Copy-Item -Path easyam.exe -Destination ../../../test/in/easyam.exe
Copy-Item -Path easyam.exe.config -Destination ../../../test/in/easyam.exe.config
Copy-Item -Path easyam.xml -Destination ../../../test/in/easyam.xml
Copy-Item -Path easyam.pdb -Destination ../../../test/in/easyam.pdb
Copy-Item -Path FSharp.Core.dll -Destination ../../../test/in/FSharp.Core.dll
Copy-Item -Path FSharp.Core.xml -Destination ../../../test/in/FSharp.Core.xml
Copy-Item -Path FSharp.Data.TypeProviders.dll -Destination ../../../test/in/FSharp.Data.TypeProviders.dll

Write-Host "setup end"

