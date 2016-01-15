import std.stdio;

dvoid main()
{
  write("Enter your name: ");
  writeln();
  write("---------------------------------");
  writeln();
  auto name = readln();
  writeln("Your name is " ~ name);
}