REM Environment variables set in appveyor.yml

if not exist %OPAM_DIR%\bin\opam.exe (
  echo Compiling opam to %OPAM_DIR% ...
  mkdir "%OPAM_DIR%"
  git clone https://github.com/ocaml/opam.git C:\temp\opam
  %CYG_ROOT%\bin\bash -lc "cd \"/cygdrive/c/temp/opam\" && env DJDIR=workaround ./configure --prefix=%OPAM_DIR% && make lib-ext && make && make install"
  goto :endopam
)
echo Using %OPAM_DIR%\bin\opam.exe ...
:endopam
