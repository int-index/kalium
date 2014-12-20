var
    i, j, x: Integer;
begin
    for i := 1 to 10 do begin
        for j := 1 to 10 do begin
            x := i*j;
            Write(x);
            if (x >=  1) and (x <=  9) then Write('   ');
            if (x >= 10) and (x <= 99) then Write('  ');
        end;
        WriteLn();
    end
end.
