import std.stdio;

void main()
{
    write("Enter your name: ");
    writeln();
    write("---------------------------------");
    writeln();
    auto name = readln();
    writeln("Your name is " ~ name);
}