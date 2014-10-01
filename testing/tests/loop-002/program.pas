function factorial(n: LongInt): LongInt;
var
    i: LongInt;
begin
    factorial := 1;
    for i := 1 to n do
        factorial := factorial * i;
end;

var
    n: LongInt;
begin
    ReadLn(n);
    n := factorial(n);
    WriteLn(n);
end.
