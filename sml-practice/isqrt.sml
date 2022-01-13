exception negError;
exception intervalError;
fun isqrt n =
  if n < 0 then raise negError
  else if n = 0 then 0
  else let fun shrink (n, l, u) = 
      if l > u orelse l * l > n orelse u * u < n
        then raise intervalError
      else if u - l = 1 then l
      else let val m = (l + u) div 2 in
        if m * m <= n then shrink (n, m, u)
        else shrink(n, l, m)
    end;
    in shrink(n, 0, n + 1)
end;
