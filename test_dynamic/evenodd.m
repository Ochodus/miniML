letrec even(x) = if (x = 0) then 1 else (odd (x-1)) and odd(x) = if (x = 0) then 0 else (even (x-1))
in (even 13)
