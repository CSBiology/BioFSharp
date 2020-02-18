@echo off
cls

dotnet tool restore
dotnet restore build.proj
dotnet paket restore
dotnet fake build %*