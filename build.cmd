@echo off
cls

dotnet tool restore
dotnet restore
dotnet paket restore
dotnet fake build %*