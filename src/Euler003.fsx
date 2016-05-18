let euler3 (n : int64) =
    let rec primeFactor number divisor =
        if number = divisor then 
            number 
        else if number % divisor = 0L then
            primeFactor (number / divisor) divisor
        else
            primeFactor number (divisor + 1L)
    primeFactor n 2L
    
euler3 600851475143L
