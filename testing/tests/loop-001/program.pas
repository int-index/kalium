var
    a, n, i: LongInt;
begin
    ReadLn(n);
    a := 1;
    for i := 1 to n do
        a := a * i;
    WriteLn(a);
end.
