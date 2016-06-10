import std.stdio;

void print_i(int i) {

    if (i < 9) {
        write(i);
        print_i(i+1);
    }
}

void main()
{
    print_i(1);
}