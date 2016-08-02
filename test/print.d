import std.stdio;

void print_i(int i)
{
    if (i < 9) {
        writeln(i);
        print_i(i+1);
    }
}

void main()
{
    print_i(1);
}