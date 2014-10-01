var
    x: LongInt;
begin
    ReadLn(x);
    if x > 0 then
        WriteLn('Positive')
    else if x < 0 then
        WriteLn('Negative')
    else
        WriteLn('Zero')
end.
