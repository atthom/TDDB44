program gege;

const
	SIZE = 10;

var
	a : array[SIZE] of integer;
{	i : integer;
	x : real; }

begin
	a[1] := 2;
	a[a[1]-1] := a[1];
 {	x := 3;
	i := trunc(x);
	x := 4 + 4/2; }

end.
