program zaher;

uses
  SysUtils;
var
  s: string;
  x: integer;
begin
  x:=10;
  writeln(x);
  x:=x+10;
  writeln(x);
  s:='Hi World';
  writeln(s);
  ReadLn();
  writeln('Bye');
  writeln('Press Enter to exit');
  ReadLn();
end.