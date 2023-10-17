Set-Location "$Env:R_RCSTAT_DEPLOY_PATH"
Start-Process PowerShell -Verb RunAs "-ExecutionPolicy Bypass -Command `"cd '$pwd'; & '$Env:R_RCSTAT_DEPLOY_PATH\check_install.ps1';`""
exit 