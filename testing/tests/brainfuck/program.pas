{
type
    IntArray = array of Integer;
}

procedure interp(s: String; var d: array of Integer; var p: Integer; i: Integer); forward;

function matching(s: String; i: Integer; depth: Integer): Integer;
begin
    if (i >= Length(s)) or (depth=0) then
        matching := i
    else begin
        case s[i] of
            '[': matching := matching(s, i+1, depth+1);
            ']': matching := matching(s, i+1, depth-1);
            else matching := matching(s, i+1, depth);
        end
    end
end;

function backmatching(s: String; i: Integer; depth: Integer): Integer;
begin
    if (i = 0) or (depth=0) then
        backmatching := i + 2
    else begin
        case s[i] of
            ']': backmatching := backmatching(s, i-1, depth+1);
            '[': backmatching := backmatching(s, i-1, depth-1);
            else backmatching := backmatching(s, i-1, depth);
        end
    end
end;

procedure interp_suck(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  p := p + 1;
  if p = Length(d) then
  begin
    SetLength(d, p+1);
    d[p] := 0;
  end;
  interp(s, d, p, i+1);
end;

procedure interp_blow(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  if p > 0 then
    p := p - 1;
  interp(s, d, p, i+1);
end;

procedure interp_plus(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  d[p] := d[p] + 1;
  interp(s, d, p, i+1);
end;

procedure interp_minus(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  d[p] := d[p] - 1;
  interp(s, d, p, i+1);
end;

procedure interp_dot(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  Write(chr(d[p]));
  interp(s, d, p, i+1);
end;

procedure interp_comma(s: String; var d: array of Integer; var p: Integer; i: Integer);
var
    x: Char;
begin
  Read(x);
  d[p] := ord(x);
  interp(s, d, p, i+1);
end;

procedure interp_open(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  if d[p] = 0 then
    i := matching(s, i+1, 1)
  else
    i := i+1;
  interp(s, d, p, i);
end;

procedure interp_close(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  if d[p] <> 0 then
    i := backmatching(s, i-1, 1)
  else
    i := i+1;
  interp(s, d, p, i);
end;

procedure interp_skip(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
  interp(s, d, p, i+1);
end;


procedure interp(s: String; var d: array of Integer; var p: Integer; i: Integer);
begin
    if i <= Length(s) then
      case s[i] of
        '>': interp_suck (s, d, p, i);
        '<': interp_blow (s, d, p, i);
        '+': interp_plus (s, d, p, i);
        '-': interp_minus(s, d, p, i);
        '.': interp_dot  (s, d, p, i);
        ',': interp_comma(s, d, p, i);
        '[': interp_open (s, d, p, i);
        ']': interp_close(s, d, p, i);
        else interp_skip (s, d, p, i);
      end
end;

var
    s: String;
    d: array of Integer;
    p: Integer;
begin
    p := 0;
    SetLength(d, 1);
    d[p] := 0;
    ReadLn(s);
    interp(s, d, p, 1);
end.
