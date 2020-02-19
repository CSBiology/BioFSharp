@echo off
cls

dotnet tool restore
dotnet paket restore
dotnet restore build.proj
dotnet fake build %*