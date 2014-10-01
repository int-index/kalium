function NPZ(x: Integer): String;
begin
    if x < 0 then
        NPZ := 'Negative'
    else if x > 0 then
        NPZ := 'Positive'
    else if x = 0 then
        NPZ := 'Zero'
end;

var
    s: String;
    x: Integer;
begin
    ReadLn(x);
    s := NPZ(x);
    WriteLn(s);
end.
