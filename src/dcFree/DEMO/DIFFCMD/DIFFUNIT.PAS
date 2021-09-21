unit DiffUnit;

interface
uses
  classes,
  sysutils,
  diffmake;

procedure MainProc;

implementation
{---------------------------------------}

procedure ShowHelp;
begin
  Writeln;
  Writeln('Use: Diff <a|e> <In_file> <out_file> <use_file> [comp_level]');
  Writeln;
  Writeln('Comp_level - 10..1000  (25 by default)');
  Writeln('            (10   - max. speed, min. comp. ratio)');
  Writeln('            (1000 - min. speed, max. comp. ratio)');
  Writeln;
  Writeln('Use_file   - old file name');
  Writeln('In_file    - input file name');
  Writeln('Out_file   - output file name');
  Writeln;
  Writeln('a - compress  e - extract');

  Writeln;
  Writeln('Press ENTER');
  Readln;
end;

{---------------------------------------}

procedure MainProc;
begin
  Writeln('Difference Maker  Version 1.36');
  Writeln('Copyright (c) 1997,1998 Dream Company');
  If (ParamCount<4) or (not (paramstr(1)[1] in ['a','A','e','E','X','x'])) then
  begin
    ShowHelp;
    exit;
  end;

  With TDIffMaker.Create(nil) do
  try
    InFile:=Paramstr(2);
    OutFile:=Paramstr(3);
    UseFile:=Paramstr(4);
    if paramcount>4 then
      CompLevel:=strtoint(paramstr(5));
    Case AnsiUpperCase(paramstr(1))[1] of
    'A':
      begin
        Writeln('Compressing...');
        Compress;
      end;
    'E','X':
      begin
        Writeln('Extracting...');
        Extract;
      end;
    end;
    Writeln;Writeln('Ok');
  finally
    Free;
  end;
end;

begin

end.