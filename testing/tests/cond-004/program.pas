var
    b: Boolean;
begin
    b := 2 * 5 - 1 = 9;
    if not b then
        WriteLn('Impossible')
    else
        b := b or (not b);
    if b and b and True or False then
        WriteLn('Hello, world!');
end.
