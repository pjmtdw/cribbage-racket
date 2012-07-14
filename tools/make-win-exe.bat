@rem Create Windows Binary

@set racket_path="C:\\Program Files\\Racket\\"
@set output_dir="cribbage"
@set collects_path="lib\\plt\\"%output_dir%"\\collects\\"
@set raco=%racket_path%raco.exe
%raco% exe -o cribbage.exe --ico cards.ico ..\\main.rkt
%raco% distribute %output_dir% cribbage.exe
xcopy /I /S /E %racket_path%collects\\games\\cards %output_dir%\\%collects_path%\\games\\cards
@pause

