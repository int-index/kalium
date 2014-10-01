function NPZ(x: Integer): String;
begin
    case x of
        -1023..-1: NPZ := 'Negative';
        +1..+1024: NPZ := 'Positive';
        0:         NPZ := 'Zero';
    end;
end;

var
    S: String;
    X: Integer;
begin
    ReadLn(X);
    S := NPZ(X);
    WriteLn(S);
end.
