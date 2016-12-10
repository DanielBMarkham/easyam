Write-Host "post-build.ps1 start"

Copy-Item -Path easyam.exe -Destination ../../../deploy/linux/easyam.exe
Copy-Item -Path easyam.exe.config -Destination ../../../deploy/linux/easyam.exe.config
Copy-Item -Path easyam.xml -Destination ../../../deploy/linux/easyam.xml
Copy-Item -Path easyam.pdb -Destination ../../../deploy/linux/easyam.pdb
Copy-Item -Path FSharp.Core.dll -Destination ../../../deploy/linux/FSharp.Core.dll
Copy-Item -Path FSharp.Core.xml -Destination ../../../deploy/linux/FSharp.Core.xml
Copy-Item -Path FSharp.Data.TypeProviders.dll -Destination ../../../deploy/linux/FSharp.Data.TypeProviders.dll


Copy-Item -Path easyam.exe -Destination        ../../../deploy/win/easyam.exe
Copy-Item -Path easyam.exe.config -Destination ../../../deploy/win/easyam.exe.config
Copy-Item -Path easyam.xml -Destination ../../../deploy/win/easyam.xml
Copy-Item -Path easyam.pdb -Destination ../../../deploy/win/easyam.pdb
Copy-Item -Path FSharp.Core.dll -Destination ../../../deploy/win/FSharp.Core.dll
Copy-Item -Path FSharp.Core.xml -Destination ../../../deploy/win/FSharp.Core.xml
Copy-Item -Path FSharp.Data.TypeProviders.dll -Destination ../../../deploy/win/FSharp.Data.TypeProviders.dll

Powershell.exe -executionpolicy remotesigned -File ..\..\..\test\scripts\setup.ps1

Write-Host "post-build.ps1 end"
