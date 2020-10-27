program test_pas;
uses
  SysUtils;
var
  s: String;
  x: Integer;
begin
  x := 10;
  Writeln(x);
  x := x + 10;
  Writeln(x);
  s := 'Hi World';
  Writeln(s);
  Writeln('Bye');
  Writeln('Press Enter to exit');
  //ReadLn();
end.
