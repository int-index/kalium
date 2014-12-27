var
    import, show, _: String;
begin
    ReadLn(import);
    if import = '' then begin
        show := 'Hi, ';
        _ := '!';
    end else begin
        _ := 'Hi, ';
        show := '!';
    end;
    if import <> '' then
        WriteLn(_, import, show)
    else
        WriteLn(show, import, _);
end.

