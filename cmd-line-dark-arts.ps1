
# change file extension of npp 2018b xml files to xls (open file and save as)
$nppPath     = 'C:\Projects\nhp_demogr_module_inputs\data-raw\npp_2018b'
$nppFilesXml = 'C:\Projects\nhp_demogr_module_inputs\data-raw\npp_2018b\*2018.xml'

Get-ChildItem $nppFilesXml | Rename-Item -NewName { $_.name -Replace '\.xml$','.xls' }

$nppFilesXls = Get-ChildItem -Path $nppPath -Filter '*2018.xls' -File
$excl        = New-Object -ComObject "Excel.Application"
$excl.DisplayAlerts = $false

foreach ($file in $nppFilesXls) {
    $wrkb = $excl.Workbooks.Open($file.FullName)
    $wrkb.SaveAs($file.FullName, 56)  # 56 is the code for .xls
    $wrkb.Close()
}

# clean-up the used COM objects
$excl.Quit()
$null = [System.Runtime.Interopservices.Marshal]::ReleaseComObject($wrkb)
$null = [System.Runtime.Interopservices.Marshal]::ReleaseComObject($excl)
[System.GC]::Collect()
[System.GC]::WaitForPendingFinalizers()
